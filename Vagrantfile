$script = <<SCRIPT
wget -q -O - https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key | apt-key add -
echo 'deb http://download.fpcomplete.com/ubuntu/vivid stable main' | sudo tee /etc/apt/sources.list.d/fpco.list
apt-get -y update
apt-get -y install libpcre3 llvm stack
stack setup
SCRIPT

Vagrant.configure("2") do |config|
    config.vm.box = "ubuntu/vivid64"
    config.vm.provision "shell", inline: $script
	config.vm.provider "virtualbox" do |v|
		v.memory = 2048
	end
end
