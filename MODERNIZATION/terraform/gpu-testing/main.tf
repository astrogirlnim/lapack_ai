terraform {
  required_version = ">= 1.0"
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}

provider "aws" {
  region = var.aws_region
}

# Data source for Ubuntu GPU AMI
data "aws_ami" "ubuntu_gpu" {
  most_recent = true
  owners      = ["099720109477"] # Canonical

  filter {
    name   = "name"
    values = ["ubuntu/images/hvm-ssd/ubuntu-22.04-amd64-server-*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }
}

# S3 bucket for test results
resource "aws_s3_bucket" "test_results" {
  bucket        = var.results_bucket_name
  force_destroy = true

  tags = {
    Name    = "AlphaTensor GPU Test Results"
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

    expiration {
      days = 30 # Keep results for 30 days
    }
  }
}

# Security group for GPU instance
resource "aws_security_group" "gpu_test" {
  name_prefix = "alphatensor-gpu-test-"
  description = "Security group for AlphaTensor GPU testing"

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
    Name = "alphatensor-gpu-test-sg"
  }
}

# Key pair for SSH access (optional)
resource "aws_key_pair" "gpu_test" {
  key_name   = "alphatensor-gpu-test-key"
  public_key = var.ssh_public_key

  tags = {
    Name = "AlphaTensor GPU Test Key"
  }
}

# IAM role for EC2 instance
resource "aws_iam_role" "gpu_test_role" {
  name = "alphatensor-gpu-test-role"

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
  name = "alphatensor-gpu-test-s3-policy"
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
  name = "alphatensor-gpu-test-profile"
  role = aws_iam_role.gpu_test_role.name
}

# GPU test instance
resource "aws_instance" "alphatensor_gpu_test" {
  ami           = data.aws_ami.ubuntu_gpu.id
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
    github_repo     = var.github_repo_url
    test_branch     = var.test_branch
    results_bucket  = aws_s3_bucket.test_results.bucket
    test_timestamp  = formatdate("YYYY-MM-DD_hhmm", timestamp())
    instance_type   = var.instance_type
  }))

  # Auto-terminate after testing
  instance_initiated_shutdown_behavior = "terminate"

  tags = {
    Name           = "AlphaTensor-GPU-Test-${formatdate("YYYY-MM-DD-hhmm", timestamp())}"
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
  name              = "/aws/ec2/alphatensor-gpu-test"
  retention_in_days = 7

  tags = {
    Name = "AlphaTensor GPU Test Logs"
  }
}
