output "instance_id" {
  description = "ID of the GPU test instance"
  value       = aws_instance.alphatensor_gpu_test.id
}

output "instance_public_ip" {
  description = "Public IP address of the GPU test instance"
  value       = aws_instance.alphatensor_gpu_test.public_ip
}

output "instance_public_dns" {
  description = "Public DNS name of the GPU test instance"
  value       = aws_instance.alphatensor_gpu_test.public_dns
}

output "instance_type" {
  description = "Instance type used for testing"
  value       = aws_instance.alphatensor_gpu_test.instance_type
}

output "gpu_configuration" {
  description = "GPU configuration details"
  value = {
    instance_type      = var.instance_type
    gpu_model         = local.instance_configs[var.instance_type].gpu_model
    gpu_memory        = local.instance_configs[var.instance_type].gpu_memory
    compute_capability = local.instance_configs[var.instance_type].compute_capability
    estimated_cost    = "${local.instance_configs[var.instance_type].hourly_cost} USD/hour"
  }
}

output "results_bucket" {
  description = "S3 bucket for test results"
  value = {
    name = aws_s3_bucket.test_results.bucket
    arn  = aws_s3_bucket.test_results.arn
    url  = "https://s3.console.aws.amazon.com/s3/buckets/${aws_s3_bucket.test_results.bucket}"
  }
}

output "cloudwatch_logs" {
  description = "CloudWatch log group for instance logs"
  value = {
    group_name = aws_cloudwatch_log_group.gpu_test_logs.name
    url        = "https://${var.aws_region}.console.aws.amazon.com/cloudwatch/home?region=${var.aws_region}#logsV2:log-groups/log-group/${replace(aws_cloudwatch_log_group.gpu_test_logs.name, "/", "$252F")}"
  }
}

output "ssh_command" {
  description = "SSH command to connect to the instance (if SSH is enabled)"
  value       = var.enable_ssh_access && var.ssh_public_key != "" ? "ssh -i ~/.ssh/alphatensor-gpu-test ubuntu@${aws_instance.alphatensor_gpu_test.public_ip}" : "SSH access disabled"
}

output "test_status_commands" {
  description = "Commands to monitor test progress"
  value = {
    check_s3_results = "aws s3 ls s3://${aws_s3_bucket.test_results.bucket}/ --recursive"
    download_results = "aws s3 cp s3://${aws_s3_bucket.test_results.bucket}/ ./test-results/ --recursive"
    watch_logs       = "aws logs tail ${aws_cloudwatch_log_group.gpu_test_logs.name} --follow"
    instance_status  = "aws ec2 describe-instances --instance-ids ${aws_instance.alphatensor_gpu_test.id} --query 'Reservations[0].Instances[0].State.Name' --output text"
  }
}

output "estimated_costs" {
  description = "Estimated costs for the test run"
  value = {
    spot_price_limit     = "${var.max_spot_price} USD/hour"
    on_demand_price     = "${local.instance_configs[var.instance_type].hourly_cost} USD/hour"
    estimated_test_cost = "${format("%.2f", tonumber(var.max_spot_price) * (var.test_timeout_minutes / 60))} USD"
    max_cost_if_timeout = "${format("%.2f", tonumber(var.max_spot_price) * (var.auto_shutdown_minutes / 60))} USD"
  }
}

output "test_configuration" {
  description = "Test configuration summary"
  value = {
    github_repo        = var.github_repo_url
    test_branch        = var.test_branch
    test_timeout       = "${var.test_timeout_minutes} minutes"
    auto_shutdown      = "${var.auto_shutdown_minutes} minutes"
    spot_instance      = "enabled"
    ssh_access         = var.enable_ssh_access ? "enabled" : "disabled"
    detailed_monitoring = var.enable_detailed_monitoring ? "enabled" : "disabled"
  }
}

output "monitoring_urls" {
  description = "URLs for monitoring the test execution"
  value = {
    ec2_console = "https://${var.aws_region}.console.aws.amazon.com/ec2/home?region=${var.aws_region}#InstanceDetails:instanceId=${aws_instance.alphatensor_gpu_test.id}"
    s3_console  = "https://s3.console.aws.amazon.com/s3/buckets/${aws_s3_bucket.test_results.bucket}"
    cloudwatch  = "https://${var.aws_region}.console.aws.amazon.com/cloudwatch/home?region=${var.aws_region}#logsV2:log-groups/log-group/${replace(aws_cloudwatch_log_group.gpu_test_logs.name, "/", "$252F")}"
  }
}

output "cleanup_commands" {
  description = "Commands to manually cleanup resources if needed"
  value = {
    terminate_instance = "aws ec2 terminate-instances --instance-ids ${aws_instance.alphatensor_gpu_test.id}"
    delete_bucket     = "aws s3 rb s3://${aws_s3_bucket.test_results.bucket} --force"
    terraform_destroy = "terraform destroy -auto-approve"
  }
}
