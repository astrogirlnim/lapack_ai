variable "aws_region" {
  description = "AWS region for the GPU test instance"
  type        = string
  default     = "us-east-1"
}

variable "instance_type" {
  description = "EC2 instance type for GPU testing"
  type        = string
  default     = "g4dn.xlarge"

  validation {
    condition = contains([
      "g4dn.xlarge",   # NVIDIA T4, 4 vCPU, 16GB RAM - $0.526/hr
      "g4dn.2xlarge",  # NVIDIA T4, 8 vCPU, 32GB RAM - $0.752/hr
      "g5.xlarge",     # NVIDIA A10G, 4 vCPU, 16GB RAM - $1.006/hr
      "g5.2xlarge",    # NVIDIA A10G, 8 vCPU, 32GB RAM - $1.212/hr
      "p3.2xlarge"     # NVIDIA V100, 8 vCPU, 61GB RAM - $3.060/hr
    ], var.instance_type)
    error_message = "Instance type must be a supported GPU instance type."
  }
}

variable "max_spot_price" {
  description = "Maximum price for spot instance (USD per hour)"
  type        = string
  default     = "0.30"
}

variable "github_repo_url" {
  description = "GitHub repository URL for the AlphaTensor project"
  type        = string
  default     = "https://github.com/YourOrg/lapack_ai.git"
}

variable "test_branch" {
  description = "Git branch to test"
  type        = string
  default     = "more-optimization-and-gpu-testing"
}

variable "results_bucket_name" {
  description = "S3 bucket name for storing test results (must be globally unique)"
  type        = string
  default     = "alphatensor-gpu-test-results"

  validation {
    condition     = can(regex("^[a-z0-9][a-z0-9-]*[a-z0-9]$", var.results_bucket_name))
    error_message = "Bucket name must be lowercase, contain only letters, numbers, and hyphens."
  }
}

variable "ssh_public_key" {
  description = "SSH public key for instance access (optional, for debugging)"
  type        = string
  default     = ""
}

variable "enable_detailed_monitoring" {
  description = "Enable detailed CloudWatch monitoring"
  type        = bool
  default     = true
}

variable "auto_shutdown_minutes" {
  description = "Auto-shutdown timeout in minutes"
  type        = number
  default     = 30

  validation {
    condition     = var.auto_shutdown_minutes >= 10 && var.auto_shutdown_minutes <= 120
    error_message = "Auto-shutdown must be between 10 and 120 minutes."
  }
}

variable "test_timeout_minutes" {
  description = "Test execution timeout in minutes"
  type        = number
  default     = 20

  validation {
    condition     = var.test_timeout_minutes >= 5 && var.test_timeout_minutes <= 60
    error_message = "Test timeout must be between 5 and 60 minutes."
  }
}

variable "enable_ssh_access" {
  description = "Enable SSH access to the instance for debugging"
  type        = bool
  default     = false
}

variable "notification_email" {
  description = "Email address for test completion notifications (optional)"
  type        = string
  default     = ""
}

variable "environment" {
  description = "Environment name (dev, staging, prod)"
  type        = string
  default     = "dev"

  validation {
    condition     = contains(["dev", "staging", "prod"], var.environment)
    error_message = "Environment must be dev, staging, or prod."
  }
}

# Local values for computed variables
locals {
  # Generate unique bucket name with timestamp
  bucket_name = var.results_bucket_name == "alphatensor-gpu-test-results" ?
    "${var.results_bucket_name}-${random_id.bucket_suffix.hex}" :
    var.results_bucket_name

  # Common tags for all resources
  common_tags = {
    Project     = "AlphaTensor"
    Component   = "GPU-Testing"
    Environment = var.environment
    ManagedBy   = "Terraform"
    CreatedAt   = timestamp()
  }

  # Instance configuration based on type
  instance_configs = {
    "g4dn.xlarge" = {
      gpu_memory   = "16GB"
      gpu_model    = "NVIDIA T4"
      compute_capability = "7.5"
      hourly_cost  = "0.526"
    }
    "g4dn.2xlarge" = {
      gpu_memory   = "16GB"
      gpu_model    = "NVIDIA T4"
      compute_capability = "7.5"
      hourly_cost  = "0.752"
    }
    "g5.xlarge" = {
      gpu_memory   = "24GB"
      gpu_model    = "NVIDIA A10G"
      compute_capability = "8.6"
      hourly_cost  = "1.006"
    }
    "g5.2xlarge" = {
      gpu_memory   = "24GB"
      gpu_model    = "NVIDIA A10G"
      compute_capability = "8.6"
      hourly_cost  = "1.212"
    }
    "p3.2xlarge" = {
      gpu_memory   = "16GB"
      gpu_model    = "NVIDIA V100"
      compute_capability = "7.0"
      hourly_cost  = "3.060"
    }
  }
}

# Random ID for unique bucket naming
resource "random_id" "bucket_suffix" {
  byte_length = 4
}
