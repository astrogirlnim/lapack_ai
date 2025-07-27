terraform {
  required_version = ">= 1.0"
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.1"
    }
  }
}

provider "aws" {
  region = var.aws_region
}

# Use a known working Ubuntu 22.04 AMI for us-east-1
locals {
  # This is a recent Ubuntu 22.04 LTS AMI that includes GPU drivers
  ubuntu_ami_id = "ami-0e86e20dae9224db8"  # Ubuntu 22.04 LTS in us-east-1
}

# S3 bucket for test results
resource "aws_s3_bucket" "test_results" {
  bucket        = var.results_bucket_name
  force_destroy = true

  tags = {
    Name    = "Nataly AlphaTensor GPU Test Results"
    Purpose = "Phase-9.2-OpenCL-Validation"
  }
}

resource "aws_s3_bucket_versioning" "test_results" {
  bucket = aws_s3_bucket.test_results.id
  versioning_configuration {
    status = "Enabled"
  }
}

resource "aws_s3_bucket_lifecycle_configuration" "test_results" {
  bucket = aws_s3_bucket.test_results.id

  rule {
    id     = "cleanup_old_results"
    status = "Enabled"

    filter {
      prefix = ""
    }

    expiration {
      days = 30 # Keep results for 30 days
    }
  }
}

# Security group for GPU instance
resource "aws_security_group" "gpu_test" {
  name_prefix = "nataly-alphatensor-gpu-test-"
  description = "Security group for Nataly AlphaTensor GPU testing"

  # SSH access (optional, for debugging)
  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  # All outbound traffic
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = {
    Name = "nataly-alphatensor-gpu-test-sg"
  }
}

# Key pair for SSH access (optional)
resource "aws_key_pair" "gpu_test" {
  key_name   = "nataly-alphatensor-gpu-test-key"
  public_key = var.ssh_public_key

  tags = {
    Name = "Nataly AlphaTensor GPU Test Key"
  }
}

# IAM role for EC2 instance
resource "aws_iam_role" "gpu_test_role" {
  name = "nataly-alphatensor-gpu-test-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "ec2.amazonaws.com"
        }
      }
    ]
  })
}

# IAM policy for S3 access
resource "aws_iam_role_policy" "gpu_test_s3_policy" {
  name = "nataly-alphatensor-gpu-test-s3-policy"
  role = aws_iam_role.gpu_test_role.id

  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Action = [
          "s3:PutObject",
          "s3:PutObjectAcl",
          "s3:GetObject"
        ]
        Resource = "${aws_s3_bucket.test_results.arn}/*"
      }
    ]
  })
}

# IAM instance profile
resource "aws_iam_instance_profile" "gpu_test_profile" {
  name = "nataly-alphatensor-gpu-test-profile"
  role = aws_iam_role.gpu_test_role.name
}

# GPU test instance
resource "aws_instance" "alphatensor_gpu_test" {
  ami           = local.ubuntu_ami_id
  instance_type = var.instance_type

  # Use spot instance for cost savings
  instance_market_options {
    market_type = "spot"
    spot_options {
      max_price                      = var.max_spot_price
      spot_instance_type             = "one-time"
      instance_interruption_behavior = "terminate"
    }
  }

  vpc_security_group_ids = [aws_security_group.gpu_test.id]
  key_name              = aws_key_pair.gpu_test.key_name
  iam_instance_profile  = aws_iam_instance_profile.gpu_test_profile.name

  # EBS root volume
  root_block_device {
    volume_size = 30
    volume_type = "gp3"
    encrypted   = true
  }

  # User data script for automated setup and testing
  user_data = base64encode(templatefile("${path.module}/setup_and_test.sh", {
    github_repo          = var.github_repo_url
    test_branch          = var.test_branch
    results_bucket       = aws_s3_bucket.test_results.bucket
    test_timestamp       = formatdate("YYYY-MM-DD_hhmm", timestamp())
    instance_type        = var.instance_type
    TEST_TIMEOUT_SECONDS = var.test_timeout_minutes * 60
  }))

  # Auto-terminate after testing
  instance_initiated_shutdown_behavior = "terminate"

  tags = {
    Name           = "Nataly-AlphaTensor-GPU-Test-${formatdate("YYYY-MM-DD-hhmm", timestamp())}"
    Purpose        = "Phase-9.2-OpenCL-Validation"
    InstanceType   = var.instance_type
    TestBranch     = var.test_branch
    AutoCleanup    = "true"
    CreatedBy      = "terraform"
  }

  # Wait for user data to complete
  provisioner "local-exec" {
    command = "echo 'GPU test instance ${self.id} launched. Monitor progress in CloudWatch logs or S3 bucket ${aws_s3_bucket.test_results.bucket}'"
  }
}

# CloudWatch log group for instance logs
resource "aws_cloudwatch_log_group" "gpu_test_logs" {
  name              = "/aws/ec2/nataly-alphatensor-gpu-test"
  retention_in_days = 7

  tags = {
    Name = "Nataly AlphaTensor GPU Test Logs"
  }
}
