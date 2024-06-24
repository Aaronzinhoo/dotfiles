. "$( pwd )/utils.sh"

PROMPT="[ RustupExtenstionLoader ]: "

echo_with_prompt "installing rustup dependencies for rust development"

rustup-init
source $HOME/.cargo/env

rustup component add rls
rustup component add rust-analysis
rustup component add rust-src
# rust watch linter
rustup component add clippy
