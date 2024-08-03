. "$( pwd )/utils.sh"

PROMPT="[ GoLangExtensionLoader ]: "

if [ -d "$GOROOT/bin" ]; then
    echo_with_prompt "Bootstrapping for golang seems to be complete already."
    echo_with_prompt "Do you wish to proceed with the install process? (y/n): "
    read resp      
    if [ ! "$resp" = 'y' ] || [ ! "$resp" = 'Y' ] ; then
	echo_with_prompt "Skipping golang bootstrapping!"
        exit 0;
    fi
fi

echo_with_prompt "Installing goenv"

brew install goenv
export PATH="$GOENV_ROOT/bin:$PATH"
eval "$(goenv init -)"
export PATH="$GOROOT/bin:$PATH"
export PATH="$PATH:$GOPATH/bin"

echo_with_prompt "Installing Go $GO_VERSION"

goenv install "$GO_VERSION"
goenv global "$GO_VERSION"

echo_with_prompt "Go $GO_VERSION installed. Current go versions include: $(goenv versions)"
echo_with_prompt "Installing go packages"

go install golang.org/x/tools/gopls@latest
go install github.com/lighttiger2505/sqls@latest
go install golang.org/x/vuln/cmd/govulncheck@latest
go install github.com/CycloneDX/cyclonedx-gomod/cmd/cyclonedx-gomod@latest

echo_with_prompt "Finished installing go packages"
