# AWS GPU Testing for AlphaTensor Phase 9.2

This project includes automated AWS GPU testing infrastructure to validate the Phase 9.2 OpenCL implementation on real NVIDIA hardware.

## 🎯 Quick Start

### Option 1: GitHub Actions (Recommended)

1. **Go to Actions tab** in your GitHub repository
2. **Click "AlphaTensor GPU Validation on AWS"**
3. **Click "Run workflow"**
4. **Configure options:**
   - Instance Type: `g4dn.xlarge` (recommended)
   - Max Spot Price: `$0.30`
   - Test Branch: `more-optimization-and-gpu-testing`
   - AWS Region: `us-east-1`
5. **Click "Run workflow"**

**Cost**: ~$0.07 per test run (20 minutes on spot instance)

### Option 2: Manual Terraform

```bash
# Navigate to infrastructure directory
cd terraform/gpu-testing

# Copy and customize configuration
cp terraform.tfvars.example terraform.tfvars
vim terraform.tfvars  # Set your GitHub repo URL and bucket name

# Initialize and deploy
terraform init
terraform plan
terraform apply

# Cleanup when done
terraform destroy
```

## 📊 What Gets Tested

✅ **OpenCL Environment**: GPU detection, platform enumeration  
✅ **Kernel Compilation**: All 49 AlphaTensor operations compile  
✅ **GPU Execution**: Actual kernel execution on NVIDIA hardware  
✅ **Accuracy Validation**: GPU vs CPU result comparison  
✅ **Performance Measurement**: Real GPU speedup metrics  

## 🔍 Expected Results

### ✅ Success (NVIDIA GPU)
```
✅ GPU detected and available
✅ GPU context initialized successfully
✅ All 49 AlphaTensor operations execute correctly
✅ Perfect accuracy maintained (< 1e-12 error)
✅ Real GPU performance measurements
```

### 🆚 vs macOS Results
- **GPU Execution**: ✅ Works (vs ❌ Apple Metal issues)
- **Kernel Creation**: ✅ Success (vs ❌ "newComputePipelineState failed")
- **Performance**: 📊 Real measurements (vs ⚠️ platform limitations)

## 💰 Cost Breakdown

| Instance | GPU | Test Cost | Use Case |
|----------|-----|-----------|----------|
| **g4dn.xlarge** | NVIDIA T4 | **$0.07** | **Recommended** |
| g5.xlarge | NVIDIA A10G | $0.12 | Latest generation |
| p3.2xlarge | NVIDIA V100 | $0.40 | High performance |

## 🚀 Benefits

1. **Real Validation**: Test on actual NVIDIA GPU hardware
2. **Platform Independence**: Distinguish implementation vs platform issues  
3. **Performance Data**: Concrete GPU speedup measurements
4. **Cost Effective**: ~$0.07 per comprehensive test run
5. **Automated**: Complete hands-off testing with results collection

## 🔧 Prerequisites

- **AWS Account** with billing configured
- **AWS CLI** credentials configured
- **GitHub repository** access (for Actions method)
- **Terraform** installed (for manual method)

## 📁 Files

- **`.github/workflows/gpu-validation.yml`** - GitHub Actions workflow
- **`terraform/gpu-testing/`** - Complete Terraform infrastructure
- **`SRC/VARIANTS/alphatensor_hybrid/test_phase_9_2.sh`** - Test suite

## 🆘 Troubleshooting

**GitHub Actions failing?**
- Check AWS credentials are configured in repository secrets
- Verify `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY` are set

**"No spot capacity" error?**
- Try different AWS region (us-west-2, eu-west-1)
- Try different instance type (g5.xlarge, g4dn.2xlarge)

**Test results not appearing?**
- Check GitHub Actions "Summary" tab for detailed results
- Results are automatically uploaded as artifacts

## 📞 Support

Check these locations for detailed information:

1. **GitHub Actions Summary** - Test results and analysis
2. **AWS CloudWatch Logs** - Detailed execution traces  
3. **S3 Bucket** - Complete test artifacts
4. **`terraform/gpu-testing/README.md`** - Comprehensive documentation

*"On real hardware tested, confident in deployment you will be."* - Yoda 🚀 
