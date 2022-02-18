#! /bin/bash
amazon-linux-extras install docker -y
service docker start
usermod -a -G docker ec2-user
chkconfig docker on
docker pull zenkavi/roptim:0.0.4

. "/etc/parallelcluster/cfnconfig"

if [[ "${cfn_node_type}" == "MasterServer" ]]
then
  mkdir /shared/.out
  mkdir /shared/.err

  export DATA_PATH=/shared/ddModels

  aws s3 sync s3://described-vs-experienced/ddModels $DATA_PATH

  chown -R ec2-user: $DATA_PATH
  chown -R ec2-user: /shared/.out
  chown -R ec2-user: /shared/.err

  echo "alias squeue='squeue -o \"%.18i %.9P %.18j %.8u %.2t %.10M %.6D %R\"'">> /home/ec2-user/.bash_profile
fi
