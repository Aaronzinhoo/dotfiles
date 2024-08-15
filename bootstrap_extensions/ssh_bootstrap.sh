. "$( pwd )/utils.sh"

PROMPT="[ GitExtensionLoader ]:"

echo_with_prompt "Attempting to make an ssh-key"

GITHUB_SSH_KEY_PATH=$HOME/.ssh/github

install_bootstrap_check "[ -f ${GITHUB_SSH_KEY_PATH}/id_ed25519 ]" "git" || exit 0;

create_github_ssh_key () {
    ssh-keygen -t ed25519 -b 4096 -f "${GITHUB_SSH_KEY_PATH}/id_ed25519"
}

cat >"$HOME/.ssh/config"<< EOF
Host github.com
  User git
  Hostname github.com
  PreferredAuthentications publickey
  IdentityFile ${GITHUB_SSH_KEY_PATH}/id_ed25519
EOF

execute_func_with_prompt create_github_ssh_key "Attempting to create ssh key for github if needed"

echo_with_prompt "Bootstrapping complete! Ensure to add public key to Github account if needed!"
