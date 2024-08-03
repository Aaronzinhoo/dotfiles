. "$( pwd )/utils.sh"

PROMPT="[ RustupExtenstionLoader ]: "

if [ -d "${RUSTUP_HOME}" ]; then
    echo_with_prompt "Bootstrapping for rust seems to be complete already."
    echo_with_prompt "Do you wish to proceed with the install process? (y/n): "
    read resp      
    if [ ! "$resp" = 'y' ] || [ ! "$resp" = 'Y' ] ; then
	echo_with_prompt "Skipping rust bootstrapping!"
        exit 0;
    fi
fi

echo_with_prompt "installing rustup dependencies for rust development"

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup-init
source $HOME/.cargo/env

rustup component add rls
rustup component add rust-analysis
rustup component add rust-src
# rust watch linter
rustup component add clippy
