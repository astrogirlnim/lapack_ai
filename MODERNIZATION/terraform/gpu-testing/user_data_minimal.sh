#!/bin/bash

# Minimal user_data script that downloads and executes the full setup script
# This avoids the 16KB AWS user_data limit

LOG_FILE="/var/log/alphatensor-gpu-test.log"

# Logging function
log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "$LOG_FILE"
}

log "Starting AlphaTensor GPU test initialization..."

# Install AWS CLI first
log "Installing AWS CLI..."
export DEBIAN_FRONTEND=noninteractive
apt-get update -y
apt-get install -y curl unzip

# Install AWS CLI v2
curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
unzip -q awscliv2.zip
./aws/install
rm -rf aws awscliv2.zip

# Download the full script from S3
log "Downloading setup script from S3..."
/usr/local/bin/aws s3 cp "s3://${results_bucket}/${setup_script_s3_key}" /tmp/setup_and_test.sh

# Make script executable
chmod +x /tmp/setup_and_test.sh

# Execute the full setup script
log "Executing full setup script..."
/tmp/setup_and_test.sh

log "User data script completed."
