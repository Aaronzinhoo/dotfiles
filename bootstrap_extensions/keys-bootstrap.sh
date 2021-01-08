. "$( pwd )/utils.sh"

PROMPT="[ WindowsExtensionLoader ]:"

echo_with_prompt "Attempting to make an ssh-key"

create_github_ssh_key () {
    ssh-keygen -t ed25519 -b 4096
}

execute_func_with_prompt create_github_ssh_keys "create ssh key for github if needed"
