# GPU Testing Infrastructure Setup
**LAPACK AI Modernization - Comprehensive GPU Testing Strategy**

## Overview

This document outlines the complete strategy for testing GPU acceleration features across multiple platforms, with emphasis on cloud-based GPU instances for continuous integration and performance validation.

## Testing Requirements

### Performance Validation Targets
- **GPU SVD**: 5-10x speedup vs CPU for matrices >1000×1000
- **AlphaTensor**: 10-20% improvement for 4×4 matrix multiplication
- **Memory Efficiency**: <50% memory overhead for GPU operations
- **Accuracy**: Maintain 1e-6 numerical precision vs CPU implementations

### Cross-Platform Support
- **NVIDIA GPUs**: RTX, Tesla, A100 series
- **AMD GPUs**: Radeon RX, Instinct series
- **Intel GPUs**: Arc, Iris Xe series
- **Cloud GPUs**: AWS, GCP, Azure instances

## Cloud GPU Testing Setup

### AWS EC2 GPU Instances

#### Instance Types and Costs

| Instance Type | GPU | vCPUs | Memory | GPU Memory | Cost/Hour* |
|---------------|-----|-------|--------|------------|------------|
| `g4dn.xlarge` | T4 | 4 | 16 GB | 16 GB | $0.526 |
| `g4dn.2xlarge` | T4 | 8 | 32 GB | 16 GB | $0.752 |
| `p3.2xlarge` | V100 | 8 | 61 GB | 16 GB | $3.06 |
| `p4d.24xlarge` | A100 | 96 | 1152 GB | 320 GB | $32.77 |

*US East prices as of 2025

#### Setup Script for AWS

```bash
#!/bin/bash
# aws_gpu_setup.sh - Launch GPU testing instance

# Configuration
INSTANCE_TYPE="g4dn.xlarge" # Change based on needs
AMI_ID="ami-0c02fb55956c7d316" # Amazon Linux 2 with NVIDIA drivers
KEY_NAME="lapack-ai-testing"
SECURITY_GROUP="lapack-ai-gpu-sg"
SUBNET_ID="subnet-xxxxxxxxx" # Replace with your subnet

# Launch instance
echo "🚀 Launching GPU testing instance..."
INSTANCE_ID=$(aws ec2 run-instances \
--image-id $AMI_ID \
--count 1 \
--instance-type $INSTANCE_TYPE \
--key-name $KEY_NAME \
--security-group-ids $SECURITY_GROUP \
--subnet-id $SUBNET_ID \
--user-data file://gpu_instance_setup.sh \
--query 'Instances[0].InstanceId' \
--output text)

echo "✅ Instance launched: $INSTANCE_ID"

# Wait for instance to be running
echo "⏳ Waiting for instance to be running..."
aws ec2 wait instance-running --instance-ids $INSTANCE_ID

# Get public IP
PUBLIC_IP=$(aws ec2 describe-instances \
--instance-ids $INSTANCE_ID \
--query 'Reservations[0].Instances[0].PublicIpAddress' \
--output text)

echo "🌐 Public IP: $PUBLIC_IP"
echo "🔗 SSH Command: ssh -i ~/.ssh/$KEY_NAME.pem ec2-user@$PUBLIC_IP"
```

#### GPU Instance Setup Script

```bash
#!/bin/bash
# gpu_instance_setup.sh - Configure GPU instance for LAPACK AI testing

set -e

echo "🔧 Setting up GPU testing environment..."

# Update system
sudo yum update -y

# Install Docker
sudo yum install -y docker
sudo systemctl start docker
sudo systemctl enable docker
sudo usermod -a -G docker ec2-user

# Install NVIDIA Container Toolkit
distribution=$(. /etc/os-release;echo $ID$VERSION_ID)
curl -s -L https://nvidia.github.io/nvidia-docker/gpgkey | sudo apt-key add -
curl -s -L https://nvidia.github.io/nvidia-docker/$distribution/nvidia-docker.list | sudo tee /etc/apt/sources.list.d/nvidia-docker.list

sudo yum install -y nvidia-container-toolkit
sudo systemctl restart docker

# Verify GPU access
echo "🎮 Testing GPU access..."
sudo docker run --rm --gpus all nvidia/cuda:11.8-base-ubuntu20.04 nvidia-smi

# Clone and setup LAPACK AI
echo "📦 Setting up LAPACK AI repository..."
git clone https://github.com/your-org/lapack_ai.git
cd lapack_ai

# Build containers
echo "🐳 Building containers..."
docker build -f MODERNIZATION/infrastructure/Dockerfile.base -t lapack-ai-base:latest .
docker build -f MODERNIZATION/infrastructure/Dockerfile.dev -t lapack-ai-dev:latest .

# Run validation
echo "✅ Running GPU validation..."
docker run --rm --gpus all -v $(pwd):/workspace --entrypoint python \
lapack-ai-dev:latest /workspace/MODERNIZATION/testing/integration_tests.py

echo "🎉 GPU testing environment ready!"
```

### Google Cloud Platform Setup

#### Instance Configuration

```yaml
# gcp_gpu_instance.yaml
name: lapack-ai-gpu-test
machineType: zones/us-central1-a/machineTypes/n1-standard-4
zone: zones/us-central1-a

disks:
- boot: true
autoDelete: true
initializeParams:
sourceImage: projects/deeplearning-platform-release/global/images/family/common-cu113
diskSizeGb: 100

guestAccelerators:
- acceleratorType: zones/us-central1-a/acceleratorTypes/nvidia-tesla-t4
acceleratorCount: 1

scheduling:
onHostMaintenance: TERMINATE
automaticRestart: false

serviceAccounts:
- email: default
scopes:
- https://www.googleapis.com/auth/cloud-platform
```

#### GCP Setup Script

```bash
#!/bin/bash
# gcp_gpu_setup.sh

# Create GPU instance
gcloud compute instances create lapack-ai-gpu-test \
--zone=us-central1-a \
--machine-type=n1-standard-4 \
--accelerator=type=nvidia-tesla-t4,count=1 \
--image-family=common-cu113 \
--image-project=deeplearning-platform-release \
--maintenance-policy=TERMINATE \
--restart-on-failure \
--metadata-from-file startup-script=gcp_startup.sh

# SSH into instance
gcloud compute ssh lapack-ai-gpu-test --zone=us-central1-a
```

### Azure GPU Setup

#### ARM Template

```json
{
"$schema": "https://schema.management.azure.com/schemas/2019-04-01/deploymentTemplate.json#",
"contentVersion": "1.0.0.0",
"resources": [
{
"type": "Microsoft.Compute/virtualMachines",
"apiVersion": "2021-03-01",
"name": "lapack-ai-gpu-vm",
"location": "East US",
"properties": {
"hardwareProfile": {
"vmSize": "Standard_NC6s_v3"
},
"storageProfile": {
"imageReference": {
"publisher": "microsoft-dsvm",
"offer": "ubuntu-hpc",
"sku": "2004",
"version": "latest"
}
}
}
}
]
}
```

## 🏠 Local GPU Testing Setup

### NVIDIA Docker Setup

```bash
#!/bin/bash
# local_gpu_setup.sh

# Install NVIDIA Docker (Ubuntu/Debian)
distribution=$(. /etc/os-release;echo $ID$VERSION_ID) \
&& curl -fsSL https://nvidia.github.io/libnvidia-container/gpgkey | sudo gpg --dearmor -o /usr/share/keyrings/nvidia-container-toolkit-keyring.gpg \
&& curl -s -L https://nvidia.github.io/libnvidia-container/$distribution/libnvidia-container.list | \
sed 's#deb https://#deb [signed-by=/usr/share/keyrings/nvidia-container-toolkit-keyring.gpg] https://#g' | \
sudo tee /etc/apt/sources.list.d/nvidia-container-toolkit.list

sudo apt-get update
sudo apt-get install -y nvidia-container-toolkit
sudo systemctl restart docker

# Test GPU access
docker run --rm --gpus all nvidia/cuda:11.8-base-ubuntu20.04 nvidia-smi
```

### macOS GPU Testing (OpenCL)

```bash
#!/bin/bash
# macos_opencl_setup.sh

# Install OpenCL development tools
brew install opencl-headers
brew install opencl-icd-loader

# Test OpenCL availability
python3 -c "
import pyopencl as cl
print('OpenCL Platforms:')
for platform in cl.get_platforms():
print(f' {platform.name}')
for device in platform.get_devices():
print(f' {device.name}')
"
```

## 🧪 Automated GPU Testing Pipeline

### GitHub Actions GPU Testing

```yaml
# .github/workflows/gpu-tests.yml
name: GPU Performance Tests

on:
push:
branches: [ main, develop ]
pull_request:
branches: [ main ]

jobs:
gpu-tests:
runs-on: [self-hosted, gpu] # Requires self-hosted GPU runner

steps:
- uses: actions/checkout@v4

- name: Build GPU containers
run: |
docker build -f MODERNIZATION/infrastructure/Dockerfile.base -t lapack-ai-base:test .
docker build -f MODERNIZATION/infrastructure/Dockerfile.dev -t lapack-ai-dev:test .

- name: Run GPU validation
run: |
docker run --rm --gpus all -v $(pwd):/workspace \
--entrypoint python lapack-ai-dev:test \
/workspace/MODERNIZATION/testing/integration_tests.py

- name: GPU performance benchmarks
run: |
docker run --rm --gpus all -v $(pwd):/workspace \
--entrypoint python lapack-ai-dev:test \
/workspace/MODERNIZATION/testing/gpu_benchmarks.py
```

### AWS Batch GPU Testing

```json
{
"jobDefinition": {
"jobDefinitionName": "lapack-ai-gpu-tests",
"type": "container",
"platformCapabilities": ["EC2"],
"containerProperties": {
"image": "your-registry/lapack-ai-dev:latest",
"vcpus": 4,
"memory": 16384,
"resourceRequirements": [
{
"type": "GPU",
"value": "1"
}
],
"jobRoleArn": "arn:aws:iam::account:role/BatchExecutionRole",
"environment": [
{
"name": "NVIDIA_VISIBLE_DEVICES",
"value": "all"
}
]
}
}
}
```

## 📊 Performance Monitoring

### GPU Metrics Collection

```python
#!/usr/bin/env python3
# gpu_metrics.py - Collect GPU performance metrics

import subprocess
import json
import time
from datetime import datetime

def collect_gpu_metrics():
"""Collect comprehensive GPU metrics."""
try:
# NVIDIA GPU metrics
result = subprocess.run([
'nvidia-smi', '--query-gpu=timestamp,name,driver_version,memory.total,memory.used,memory.free,utilization.gpu,utilization.memory,temperature.gpu',
'--format=csv,noheader,nounits'
], capture_output=True, text=True)

if result.returncode == 0:
lines = result.stdout.strip().split('\n')
metrics = []

for line in lines:
values = [v.strip() for v in line.split(',')]
metrics.append({
'timestamp': values[0],
'gpu_name': values[1],
'driver_version': values[2],
'memory_total_mb': int(values[3]),
'memory_used_mb': int(values[4]),
'memory_free_mb': int(values[5]),
'gpu_utilization_percent': int(values[6]),
'memory_utilization_percent': int(values[7]),
'temperature_c': int(values[8])
})

return metrics

except Exception as e:
print(f"Error collecting GPU metrics: {e}")
return []

def benchmark_gpu_operations():
"""Benchmark GPU operations vs CPU."""
import numpy as np
import time

# Test matrix sizes
sizes = [100, 500, 1000, 2000]
results = {}

for size in sizes:
print(f"Testing {size}x{size} matrices...")

# Generate test matrix
A = np.random.rand(size, size).astype(np.float64)

# CPU SVD timing
start_time = time.time()
U_cpu, s_cpu, Vt_cpu = np.linalg.svd(A)
cpu_time = time.time() - start_time

# GPU SVD timing (placeholder - implement with GPU code)
# start_time = time.time()
# U_gpu, s_gpu, Vt_gpu = gpu_svd(A)
# gpu_time = time.time() - start_time

results[size] = {
'cpu_time': cpu_time,
'cpu_memory_usage': A.nbytes * 4, # Rough estimate
# 'gpu_time': gpu_time,
# 'speedup': cpu_time / gpu_time
}

print(f" CPU: {cpu_time:.3f}s")

return results

if __name__ == "__main__":
print("🎮 GPU Performance Monitoring")
print("=" * 40)

# Collect GPU metrics
metrics = collect_gpu_metrics()
if metrics:
print("GPU Information:")
for gpu in metrics:
print(f" {gpu['gpu_name']}: {gpu['gpu_utilization_percent']}% utilization")

# Run benchmarks
print("\nRunning benchmarks...")
benchmark_results = benchmark_gpu_operations()

# Save results
timestamp = datetime.now().isoformat()
results = {
'timestamp': timestamp,
'gpu_metrics': metrics,
'benchmark_results': benchmark_results
}

with open(f'gpu_test_results_{timestamp.replace(":", "-")}.json', 'w') as f:
json.dump(results, f, indent=2)

print("✅ Results saved to gpu_test_results_*.json")
```

## 🔧 Testing Scripts

### Comprehensive GPU Test Suite

```bash
#!/bin/bash
# run_gpu_tests.sh - Complete GPU testing pipeline

set -e

echo "🎮 LAPACK AI GPU Testing Pipeline"
echo "================================="

# Configuration
CONTAINER_TAG="lapack-ai-dev:latest"
RESULTS_DIR="./gpu_test_results"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)

# Create results directory
mkdir -p $RESULTS_DIR

# Test 1: Environment validation
echo "🔍 Test 1: Environment Validation"
docker run --rm --gpus all -v $(pwd):/workspace \
--entrypoint python $CONTAINER_TAG \
/workspace/MODERNIZATION/testing/environment_validation.py \
> $RESULTS_DIR/environment_validation_$TIMESTAMP.log 2>&1

# Test 2: Integration tests
echo "🧪 Test 2: Integration Tests"
docker run --rm --gpus all -v $(pwd):/workspace \
--entrypoint python $CONTAINER_TAG \
/workspace/MODERNIZATION/testing/integration_tests.py \
> $RESULTS_DIR/integration_tests_$TIMESTAMP.log 2>&1

# Test 3: Performance benchmarks
echo "⚡ Test 3: Performance Benchmarks"
docker run --rm --gpus all -v $(pwd):/workspace \
--entrypoint python $CONTAINER_TAG \
/workspace/MODERNIZATION/testing/gpu_metrics.py \
> $RESULTS_DIR/gpu_benchmarks_$TIMESTAMP.log 2>&1

# Test 4: Memory stress tests
echo "💾 Test 4: Memory Stress Tests"
docker run --rm --gpus all -v $(pwd):/workspace \
--entrypoint python $CONTAINER_TAG \
-c "
import numpy as np
print('Testing large matrix operations...')
for size in [5000, 10000, 15000]:
try:
A = np.random.rand(size, size).astype(np.float32)
U, s, Vt = np.linalg.svd(A)
print(f'{size}x{size}: ✅ Success')
del A, U, s, Vt
except Exception as e:
print(f'{size}x{size}: ❌ Failed - {e}')
" > $RESULTS_DIR/memory_stress_$TIMESTAMP.log 2>&1

echo "✅ All tests completed!"
echo "📁 Results saved to: $RESULTS_DIR/"
echo "📊 Summary:"
grep -c "✅\|PASS" $RESULTS_DIR/*_$TIMESTAMP.log | head -10
```

## 💰 Cost Optimization

### Spot Instance Usage

```bash
#!/bin/bash
# aws_spot_gpu.sh - Use spot instances for cost savings

# Request spot instance (up to 70% cost savings)
aws ec2 request-spot-instances \
--spot-price "0.50" \
--instance-count 1 \
--type "one-time" \
--launch-specification '{
"ImageId": "ami-0c02fb55956c7d316",
"InstanceType": "g4dn.xlarge",
"KeyName": "lapack-ai-testing",
"SecurityGroupIds": ["sg-xxxxxxxxx"],
"UserData": "'$(base64 -w 0 gpu_instance_setup.sh)'"
}'
```

### Auto-shutdown Script

```bash
#!/bin/bash
# auto_shutdown.sh - Automatically shutdown after tests

# Add to instance user data
echo "#!/bin/bash
# Auto-shutdown after 2 hours if no active processes
(sleep 7200 && sudo shutdown -h now) &

# Monitor for active testing
while true; do
if ! pgrep -f 'python.*test'; then
echo 'No active tests detected, shutting down in 10 minutes...'
sleep 600
sudo shutdown -h now
fi
sleep 300
done" | sudo tee /etc/rc.local
sudo chmod +x /etc/rc.local
```

## 📋 Testing Checklist

### Pre-deployment Validation

- [ ] **GPU Detection**: OpenCL platforms enumerated
- [ ] **Container Build**: All containers build successfully
- [ ] **Memory Access**: GPU memory allocation working
- [ ] **Kernel Compilation**: OpenCL kernels compile without errors
- [ ] **Data Transfer**: CPU ↔ GPU memory transfer functional
- [ ] **Fallback**: CPU fallback triggers correctly without GPU

### Performance Validation

- [ ] **SVD Speedup**: 5-10x improvement over CPU
- [ ] **AlphaTensor**: 10-20% improvement for 4×4 matrices
- [ ] **Memory Efficiency**: <50% overhead vs CPU
- [ ] **Accuracy**: 1e-6 precision maintained
- [ ] **Stability**: No memory leaks over extended runs
- [ ] **Scalability**: Performance scales with matrix size

### Cross-Platform Testing

- [ ] **NVIDIA GPUs**: RTX, Tesla, A100 series
- [ ] **AMD GPUs**: Radeon RX, Instinct series
- [ ] **Intel GPUs**: Arc, Iris Xe series
- [ ] **Cloud Platforms**: AWS, GCP, Azure
- [ ] **Container Environments**: Docker, Kubernetes
- [ ] **Operating Systems**: Ubuntu, CentOS, Amazon Linux

---

**Next Steps**:
1. Choose your preferred cloud platform (AWS recommended for cost-effectiveness)
2. Set up GPU testing instance using provided scripts
3. Run comprehensive test suite and collect performance baselines
4. Integrate GPU testing into CI/CD pipeline

**Estimated Monthly Cost**: $50-200 depending on usage patterns and spot instance availability.
