. "$( pwd )/utils.sh"

PROMPT="[ GoLangExtensionLoader ]: "

echo_with_prompt "installing gopls server for emacs"

# May need GO111MODULE=on go get golang.org/x/tools/gopls@latest
go install github.com/kevincobain2000/gobrew/cmd/gobrew@latest
gobrew use latest

go install golang.org/x/tools/gopls@latest
go install github.com/lighttiger2505/sqls@latest
go install golang.org/x/vuln/cmd/govulncheck@latest
go install github.com/CycloneDX/cyclonedx-gomod/cmd/cyclonedx-gomod@latest
