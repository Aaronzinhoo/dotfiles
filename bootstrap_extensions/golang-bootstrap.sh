. "$( pwd )/utils.sh"

PROMPT="[ GoLangExtensionLoader ]: "

echo_with_prompt "installing gopls server for emacs"

# May need GO111MODULE=on go get golang.org/x/tools/gopls@latest
go install golang.org/x/tools/gopls@latest
go install github.com/lighttiger2505/sqls@latest
