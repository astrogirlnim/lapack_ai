# AlphaTensor AWS GPU Testing Infrastructure

This directory contains Terraform infrastructure for automated GPU testing of the AlphaTensor Phase 9.2 OpenCL implementation on AWS.

## 🎯 Purpose

Test your AlphaTensor OpenCL implementation on **real NVIDIA GPU hardware** to validate that it works correctly beyond the Apple Metal compatibility limitations we encountered on macOS.

## 🚀 Quick Start

### Prerequisites

1. **AWS Account** with billing configured
2. **AWS CLI** configured with appropriate permissions
3. **Terraform** >= 1.0 installed
4. **GitHub repository** with your AlphaTensor code

### 1. Setup AWS Credentials

```bash
# Configure AWS CLI
aws configure

# Or export credentials
export AWS_ACCESS_KEY_ID="your-access-key"
export AWS_SECRET_ACCESS_KEY="your-secret-key"
```

### 2. Configure Variables

```bash
# Copy example configuration
cp terraform.tfvars.example terraform.tfvars

# Edit with your settings
vim terraform.tfvars
```

Required customizations:
- `github_repo_url`: Your repository URL
- `results_bucket_name`: Globally unique S3 bucket name

### 3. Deploy and Test

```bash
# Initialize Terraform
terraform init

# Plan deployment
terraform plan

# Deploy and run tests
terraform apply

# Monitor progress
aws s3 ls s3://your-results-bucket/ --recursive
```

### 4. Cleanup

```bash
# Always cleanup to avoid charges
terraform destroy
```

## 💰 Cost Optimization

| Instance Type | GPU | On-Demand | Spot Price | Test Cost |
|---------------|-----|-----------|------------|-----------|
| **g4dn.xlarge** | NVIDIA T4 | $0.526/hr | ~$0.20/hr | **$0.07** |
| g4dn.2xlarge | NVIDIA T4 | $0.752/hr | ~$0.25/hr | $0.08 |
| g5.xlarge | NVIDIA A10G | $1.006/hr | ~$0.35/hr | $0.12 |
| p3.2xlarge | NVIDIA V100 | $3.060/hr | ~$1.20/hr | $0.40 |

**Recommendation**: Use `g4dn.xlarge` for cost-effective testing (~$0.07 per test run)

## 📊 What Gets Tested

The infrastructure automatically:

1. **🏗️ Environment Setup**
   - Launches GPU instance with NVIDIA drivers
   - Installs OpenCL development environment
   - Clones your repository and builds LAPACK

2. **🧪 AlphaTensor Testing**
   - Runs Phase 9.2 comprehensive test suite
   - Tests OpenCL kernel compilation
   - Validates GPU vs CPU accuracy
   - Measures performance benchmarks

3. **📈 Results Collection**
   - Uploads all results to S3
   - Generates summary reports
   - Auto-shutdown for cost control

## 🔧 Manual Testing (Alternative)

If you prefer manual control:

```bash
# Deploy infrastructure only
terraform apply -target=aws_instance.alphatensor_gpu_test

# SSH to instance (if SSH enabled)
ssh -i ~/.ssh/alphatensor-gpu-test ubuntu@<instance-ip>

# Run tests manually
cd /tmp/lapack_ai/SRC/VARIANTS/alphatensor_hybrid
./test_phase_9_2.sh

# Cleanup when done
terraform destroy
```

## 📋 Expected Results

### ✅ Success Case (NVIDIA GPU)
```
TEST 1: OpenCL Environment Validation
======================================
✅ GPU detected and available
✅ GPU context initialized successfully

TEST 2: Single 4x4 Matrix Accuracy Testing
==========================================
✅ Identity test - Max error: 1.42E-14
✅ Random test - Max error: 3.67E-13
✅ Edge case test - Max error: 0.00E+00

TEST 3: Basic Performance Check
===============================
✅ GPU execution successful

✅ OVERALL RESULT: ALL TESTS PASSED
✅ Phase 9.2 OpenCL implementation VERIFIED
```

### ⚠️ Expected Differences from macOS
- **GPU Execution**: Should work (vs Apple Metal issues)
- **OpenCL Compilation**: Should succeed fully
- **Performance**: Real GPU speedup measurements
- **Kernel Creation**: No "newComputePipelineState failed" errors

## 📁 Infrastructure Components

### Terraform Files
- `main.tf` - Core infrastructure (EC2, S3, IAM)
- `variables.tf` - Input variables and validation
- `outputs.tf` - Resource outputs and monitoring URLs
- `setup_and_test.sh` - Automated setup script for GPU instance

### Key Features
- **Spot Instances**: 60% cost savings with automatic bidding
- **Auto-Shutdown**: Prevents runaway costs with forced termination
- **S3 Results**: Persistent storage of all test artifacts
- **CloudWatch Logs**: Centralized logging and monitoring
- **SSH Access**: Optional debugging access to GPU instance

## 🔐 Security Considerations

- **IAM Roles**: Minimal permissions for S3 access only
- **Security Groups**: SSH access only if explicitly enabled
- **Encryption**: EBS volumes encrypted at rest
- **Temporary**: All resources auto-cleanup after testing

## 🚨 Cost Protection

1. **Spot Instances**: Default max price $0.30/hour
2. **Auto-Shutdown**: Force shutdown after 30 minutes
3. **Test Timeout**: Fail-safe timeout after 20 minutes
4. **S3 Lifecycle**: Results deleted after 30 days
5. **Terraform Destroy**: Always cleanup infrastructure

## 🛠️ Troubleshooting

### Common Issues

**"No spot capacity"**
```bash
# Try different region or instance type
terraform apply -var="aws_region=us-west-2" -var="instance_type=g5.xlarge"
```

**"SSH connection refused"**
```bash
# Wait for instance initialization (2-3 minutes)
# Check security group allows SSH from your IP
```

**"Test results not found"**
```bash
# Check S3 bucket
aws s3 ls s3://your-bucket-name/ --recursive

# Check CloudWatch logs
aws logs tail /aws/ec2/alphatensor-gpu-test --follow
```

### Debugging Commands

```bash
# Check instance status
aws ec2 describe-instances --instance-ids i-1234567890abcdef0

# Download all results
aws s3 cp s3://your-bucket/ ./gpu-test-results/ --recursive

# View execution logs
aws logs tail /aws/ec2/alphatensor-gpu-test --follow

# Connect via SSH (if enabled)
ssh -i ~/.ssh/alphatensor-gpu-test ubuntu@<instance-ip>
```

## 🔄 Integration with GitHub Actions

This infrastructure can be triggered automatically via GitHub Actions:

1. Manual trigger from GitHub UI
2. Automatic testing on PR merge
3. Scheduled nightly testing
4. Cost-controlled with approval gates

See `.github/workflows/gpu-validation.yml` for the complete workflow.

## 📞 Support

If you encounter issues:

1. **Check AWS CloudWatch logs** for detailed execution traces
2. **Review S3 results** for partial test outputs
3. **Enable SSH access** for interactive debugging
4. **Try different regions/instance types** for availability

## 🎉 Expected Impact

This infrastructure will:

- ✅ **Validate** your Phase 9.2 implementation works on real GPU hardware
- ✅ **Distinguish** between implementation issues vs platform limitations  
- ✅ **Provide** concrete performance measurements vs CPU
- ✅ **Enable** confident deployment of GPU-accelerated AlphaTensor
- ✅ **Cost-effectively** test with automatic cleanup (~$0.07 per test)

*"Validated on real hardware, your implementation will be. Trust in the cloud, you must."* - Yoda ☁️🚀 
