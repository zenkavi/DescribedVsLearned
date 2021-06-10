
Notes based on this tutorial: https://www.hpcworkshops.com/

# Cloud9 environment

- Use AWS Management Console > Cloud9 to create environment.

- Install/update AWS CLI
```
pip3 install awscli -U --user
```

# S3
[In cloud9 environment]

- Create S3 bucket
```
BUCKET_POSTFIX=$(uuidgen --random | cut -d'-' -f1)
aws s3 mb s3://bucket-${BUCKET_POSTFIX}
```

- Upload file to s3 bucket
```
aws s3 cp ./SEG_C3NA_Velocity.sgy s3://bucket-${BUCKET_POSTFIX}/SEG_C3NA_Velocity.sgy
```

- Check bucket contents
```
aws s3 ls s3://bucket-${BUCKET_POSTFIX}/
```

# EC2
[In cloud9 environment]

- Generate a key pair for SSH access into the EC2 instance
```
aws ec2 create-key-pair --key-name lab-2-your-key --query KeyMaterial --output text > ~/.ssh/id_rsa
chmod 600 ~/.ssh/id_rsa
```

- Identify the Subnet ID and VPC (virtual private cloud) ID of the cloud9 instance
```
MAC=$(curl -s http://169.254.169.254/latest/meta-data/network/interfaces/macs/)
cat << EOF
***********************************************************************************
Subnet ID = $(curl -s http://169.254.169.254/latest/meta-data/network/interfaces/macs/$MAC/subnet-id)
VPC ID = $(curl -s http://169.254.169.254/latest/meta-data/network/interfaces/macs/$MAC/vpc-id)
************************************************************************************
EOF
```

- List information on instances
```
aws ec2 describe-instances --query 'Reservations[*].Instances[*].[Tags[?Key==`Name`]| [0].Value,InstanceType, PrivateIpAddress, PublicIpAddress]' --filters Name=instance-state-name,Values=running --output table
```

- To give EC2 access to S3
  - create an IAM role with `AmazonS3FullAccess` policy
  - then in EC2 > Instances > select instance > Actions > Security > Modify IAM role

# Parallel Cluster
[In cloud9 environment]
