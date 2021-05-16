. "$( pwd )/utils.sh"

PROMPT="[ RustupExtenstionLoader ]: "

echo_with_prompt "installing rustup dependencies for rust development"

rustup component add rls --toolchain nightly
rustup component add rust-analysis --toolchain nightly
rustup component add rust-src --toolchain nightly
