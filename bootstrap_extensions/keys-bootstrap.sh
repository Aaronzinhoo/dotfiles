. "$( pwd )/utils.sh"

create_github_ssh_key(){
    ssh-keygen -t ed25519 -b 4096
}

echo_func_with_propmt create_github_ssh_keys "create ssh key for github if needed"
