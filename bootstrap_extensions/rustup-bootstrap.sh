. "$( pwd )/utils.sh"

PROMPT="[ RustupExtenstionLoader ]: "

echo_with_prompt "installing rustup dependencies for rust development"

rustup-init
source $HOME/.cargo/env

rustup component add rls --toolchain nightly
rustup component add rust-analysis --toolchain nightly
rustup component add rust-src --toolchain nightly
# rust watch linter
rustup component add clippy
