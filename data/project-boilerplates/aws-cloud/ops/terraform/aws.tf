# Declare an AWS provider
# =======================
provider "aws" {
  version = "~> 2.0"
}

# Common AWS data
# ===============
data "aws_caller_identity" "current" {}
data "aws_availability_zones" "available" {}
data "aws_region" "current" {}
