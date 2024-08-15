. "$( pwd )/utils.sh"

PROMPT="[ RustExtenstionLoader ]: "

install_bootstrap_check "[ -d ${RUSTUP_HOME} ]" "rust" || exit 0;
echo_with_prompt "installing rustup dependencies for rust development"

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup-init
source "${CARGO_HOME}/cargo/env"

rustup component add rls
rustup component add rust-analysis
rustup component add rust-src
# rust watch linter
rustup component add clippy
