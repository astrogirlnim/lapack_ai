#!/bin/bash

# =================================================================
# ALPHATENSOR GPU TESTING SETUP AND EXECUTION SCRIPT
# =================================================================
#
# This script runs on AWS GPU instances to:
# 1. Install OpenCL development environment
# 2. Clone and build the AlphaTensor repository
# 3. Execute Phase 9.2 GPU tests
# 4. Upload results to S3
# 5. Auto-shutdown the instance
#
# Template variables:
# - github_repo: ${github_repo}
# - test_branch: ${test_branch}
# - results_bucket: ${results_bucket}
# - test_timestamp: ${test_timestamp}
# - instance_type: ${instance_type}

set -e  # Exit on any error
set -o pipefail  # Exit on pipe failures

# =================================================================
# CONFIGURATION AND LOGGING SETUP
# =================================================================

LOG_FILE="/var/log/alphatensor-gpu-test.log"
RESULTS_DIR="/tmp/alphatensor-test-results"
REPO_DIR="/tmp/lapack_ai"
START_TIME=$(date +%s)
TEST_TIMEOUT_SECONDS=$((20 * 60))  # 20 minutes timeout

# Create results directory
mkdir -p "$RESULTS_DIR"

# Logging function
log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "$LOG_FILE"
}

# Error handling function
handle_error() {
    local exit_code=$?
    log "ERROR: Script failed with exit code $exit_code at line $1"
    upload_error_logs
    schedule_shutdown 5
    exit $exit_code
}

# Set up error trapping
trap 'handle_error $LINENO' ERR

log "==================================================================="
log "ALPHATENSOR GPU TESTING STARTED"
log "==================================================================="
log "Instance Type: ${instance_type}"
log "GitHub Repo: ${github_repo}"
log "Test Branch: ${test_branch}"
log "Results Bucket: ${results_bucket}"
log "Test Timestamp: ${test_timestamp}"
log "Log File: $LOG_FILE"

# =================================================================
# SYSTEM INFORMATION COLLECTION
# =================================================================

log "Collecting system information..."

# System info
uname -a > "$RESULTS_DIR/system_info.txt"
lscpu >> "$RESULTS_DIR/system_info.txt"
free -h >> "$RESULTS_DIR/system_info.txt"
df -h >> "$RESULTS_DIR/system_info.txt"

# GPU info (will be populated after driver installation)
echo "GPU information will be updated after driver installation" > "$RESULTS_DIR/gpu_info.txt"

# =================================================================
# PACKAGE INSTALLATION AND SYSTEM SETUP
# =================================================================

log "Updating system packages..."
export DEBIAN_FRONTEND=noninteractive
apt-get update -y

log "Installing basic development tools..."
apt-get install -y \
    wget \
    curl \
    git \
    build-essential \
    cmake \
    pkg-config \
    awscli \
    unzip \
    htop \
    vim

# =================================================================
# NVIDIA GPU DRIVER AND OPENCL SETUP
# =================================================================

log "Installing NVIDIA GPU drivers and OpenCL..."

# Install NVIDIA driver
apt-get install -y nvidia-driver-535 nvidia-dkms-535

# Install OpenCL development environment
apt-get install -y \
    nvidia-opencl-dev \
    opencl-headers \
    ocl-icd-opencl-dev \
    clinfo

# Install build tools
apt-get install -y \
    gfortran \
    gcc \
    g++ \
    libblas-dev \
    liblapack-dev \
    libopenblas-dev

log "Waiting for GPU driver initialization..."
sleep 30

# Check GPU and OpenCL status
log "Verifying GPU and OpenCL installation..."
nvidia-smi > "$RESULTS_DIR/gpu_info.txt" 2>&1 || echo "nvidia-smi failed" >> "$RESULTS_DIR/gpu_info.txt"
clinfo > "$RESULTS_DIR/opencl_info.txt" 2>&1 || echo "clinfo failed" >> "$RESULTS_DIR/opencl_info.txt"

# Verify OpenCL is working
if clinfo | grep -q "Platform Name"; then
    log "SUCCESS: OpenCL platforms detected"
    clinfo | grep -E "(Platform Name|Device Name|Device Type)" | tee -a "$LOG_FILE"
else
    log "WARNING: OpenCL not properly detected"
fi

# =================================================================
# REPOSITORY CLONE AND BUILD
# =================================================================

log "Cloning AlphaTensor repository..."
cd /tmp

# Clone repository with retry logic
for attempt in 1 2 3; do
    if git clone "${github_repo}" "$REPO_DIR"; then
        log "Successfully cloned repository on attempt $attempt"
        break
    else
        log "Clone attempt $attempt failed, retrying..."
        rm -rf "$REPO_DIR"
        sleep 10
    fi
done

if [ ! -d "$REPO_DIR" ]; then
    log "ERROR: Failed to clone repository after 3 attempts"
    exit 1
fi

cd "$REPO_DIR"

# Checkout test branch
if git checkout "${test_branch}"; then
    log "Successfully checked out branch: ${test_branch}"
else
    log "WARNING: Failed to checkout branch ${test_branch}, using current branch"
fi

# Get commit information
git log --oneline -n 5 > "$RESULTS_DIR/commit_info.txt"
git status >> "$RESULTS_DIR/commit_info.txt"

log "Building LAPACK/BLAS dependencies..."

# Create build directory
mkdir -p build
cd build

# Configure with CMake
if cmake .. -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON; then
    log "CMake configuration successful"
else
    log "ERROR: CMake configuration failed"
    exit 1
fi

# Build with parallel jobs
if make -j$(nproc); then
    log "Build successful"
else
    log "ERROR: Build failed"
    exit 1
fi

# Set library path
export LD_LIBRARY_PATH="$REPO_DIR/build/lib:$LD_LIBRARY_PATH"
echo "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH" >> ~/.bashrc

# =================================================================
# ALPHATENSOR GPU TESTING EXECUTION
# =================================================================

log "Starting AlphaTensor GPU tests..."
cd "$REPO_DIR/SRC/VARIANTS/alphatensor_hybrid"

# Verify test files exist
if [ ! -f "test_phase_9_2.sh" ]; then
    log "ERROR: test_phase_9_2.sh not found"
    ls -la >> "$LOG_FILE"
    exit 1
fi

# Make test script executable
chmod +x test_phase_9_2.sh

# Set timeout for test execution
log "Executing Phase 9.2 tests with $((TEST_TIMEOUT_SECONDS / 60)) minute timeout..."

# Run tests with timeout
timeout ${TEST_TIMEOUT_SECONDS}s ./test_phase_9_2.sh > "$RESULTS_DIR/test_output.log" 2>&1
TEST_EXIT_CODE=$?

if [ $TEST_EXIT_CODE -eq 0 ]; then
    log "SUCCESS: Tests completed successfully"
elif [ $TEST_EXIT_CODE -eq 124 ]; then
    log "WARNING: Tests timed out after $((TEST_TIMEOUT_SECONDS / 60)) minutes"
else
    log "WARNING: Tests completed with exit code $TEST_EXIT_CODE"
fi

# Copy additional test artifacts
cp -f test_output.log "$RESULTS_DIR/" 2>/dev/null || true
cp -f *.o "$RESULTS_DIR/" 2>/dev/null || true
cp -f test_phase_9_2 "$RESULTS_DIR/" 2>/dev/null || true

# =================================================================
# PERFORMANCE BENCHMARKING
# =================================================================

log "Running additional performance benchmarks..."

# Create simple performance test
cat > "$RESULTS_DIR/gpu_benchmark.sh" << 'EOF'
#!/bin/bash
echo "=== GPU Performance Benchmark ==="
echo "Date: $(date)"
echo "Instance: ${instance_type}"

# GPU memory bandwidth test
if command -v nvidia-smi &> /dev/null; then
    echo "=== GPU Memory Info ==="
    nvidia-smi --query-gpu=memory.total,memory.used,memory.free --format=csv

    echo "=== GPU Utilization ==="
    nvidia-smi --query-gpu=utilization.gpu,utilization.memory --format=csv
fi

# OpenCL device info
if command -v clinfo &> /dev/null; then
    echo "=== OpenCL Device Info ==="
    clinfo | grep -E "(Device Name|Max compute units|Max work group size|Max memory)"
fi
EOF

chmod +x "$RESULTS_DIR/gpu_benchmark.sh"
bash "$RESULTS_DIR/gpu_benchmark.sh" > "$RESULTS_DIR/gpu_benchmark_results.txt" 2>&1

# =================================================================
# RESULTS COLLECTION AND ANALYSIS
# =================================================================

log "Collecting and analyzing test results..."

# Create summary report
cat > "$RESULTS_DIR/test_summary.json" << EOF
{
  "test_execution": {
    "timestamp": "${test_timestamp}",
    "instance_type": "${instance_type}",
    "test_branch": "${test_branch}",
    "start_time": "$START_TIME",
    "end_time": "$(date +%s)",
    "duration_seconds": "$(($(date +%s) - START_TIME))",
    "exit_code": "$TEST_EXIT_CODE"
  },
  "test_results": {
    "phase_9_2_status": "$(grep -o 'OVERALL RESULT: .*' "$RESULTS_DIR/test_output.log" || echo 'UNKNOWN')",
    "tests_passed": "$(grep -o 'Tests Passed: .*' "$RESULTS_DIR/test_output.log" || echo 'UNKNOWN')",
    "gpu_available": "$(grep -q 'GPU detected and available' "$RESULTS_DIR/test_output.log" && echo 'true' || echo 'false')",
    "opencl_working": "$(grep -q 'Successfully compiled AlphaTensor kernels' "$RESULTS_DIR/test_output.log" && echo 'true' || echo 'false')"
  },
  "system_info": {
    "gpu_model": "$(nvidia-smi --query-gpu=gpu_name --format=csv,noheader 2>/dev/null || echo 'Unknown')",
    "gpu_memory": "$(nvidia-smi --query-gpu=memory.total --format=csv,noheader 2>/dev/null || echo 'Unknown')",
    "opencl_platforms": "$(clinfo 2>/dev/null | grep -c 'Platform Name' || echo '0')"
  }
}
EOF

# Create comprehensive results archive
cd "$RESULTS_DIR"
tar -czf "alphatensor_gpu_test_${test_timestamp}.tar.gz" *.txt *.log *.json *.sh 2>/dev/null || true

log "Test execution completed. Results collected in $RESULTS_DIR"
ls -la "$RESULTS_DIR" >> "$LOG_FILE"

# =================================================================
# UPLOAD RESULTS TO S3
# =================================================================

upload_results() {
    log "Uploading results to S3 bucket: ${results_bucket}"

    for file in "$RESULTS_DIR"/*; do
        if [ -f "$file" ]; then
            filename=$(basename "$file")
            s3_key="${test_timestamp}_${filename}"

            if aws s3 cp "$file" "s3://${results_bucket}/$s3_key"; then
                log "Uploaded: $filename -> s3://${results_bucket}/$s3_key"
            else
                log "WARNING: Failed to upload $filename"
            fi
        fi
    done

    # Upload main log file
    aws s3 cp "$LOG_FILE" "s3://${results_bucket}/${test_timestamp}_execution.log" || true
}

upload_error_logs() {
    log "Uploading error logs to S3..."
    aws s3 cp "$LOG_FILE" "s3://${results_bucket}/${test_timestamp}_error.log" 2>/dev/null || true

    # Upload any available results even on error
    for file in "$RESULTS_DIR"/*.txt "$RESULTS_DIR"/*.log; do
        if [ -f "$file" ]; then
            aws s3 cp "$file" "s3://${results_bucket}/" 2>/dev/null || true
        fi
    done
}

# Upload results
upload_results

# =================================================================
# CLEANUP AND SHUTDOWN
# =================================================================

schedule_shutdown() {
    local minutes=${1:-5}
    log "Scheduling system shutdown in $minutes minutes..."
    echo "AlphaTensor GPU testing completed. System will shutdown in $minutes minutes." | wall
    shutdown -h +$minutes
}

# Final status report
TOTAL_DURATION=$(($(date +%s) - START_TIME))
log "==================================================================="
log "ALPHATENSOR GPU TESTING COMPLETED"
log "==================================================================="
log "Total Duration: $((TOTAL_DURATION / 60)) minutes $((TOTAL_DURATION % 60)) seconds"
log "Test Exit Code: $TEST_EXIT_CODE"
log "Results uploaded to: s3://${results_bucket}/"
log "Instance will auto-shutdown in 5 minutes"
log "==================================================================="

# Schedule auto-shutdown
schedule_shutdown 5

log "Setup and test script completed successfully."
