. "$( pwd )/utils.sh"

PROMPT="[ KubernetesExtensionLoader ]: "

if kubectl krew > /dev/null; then
    echo_with_prompt "Bootstrapping for kubernetes seems to be complete already."
    echo_with_prompt "Do you wish to proceed with the install process? (y/n): "
    read resp      
    if [ ! "$resp" = 'y' ] || [ ! "$resp" = 'Y' ] ; then
	echo_with_prompt "Skipping kubernetes bootstrapping!"
        exit 0;
    fi
fi

echo_with_prompt "Installing Krew and friends!"

set -x; cd "$(mktemp -d)" &&
    OS="$(uname | tr '[:upper:]' '[:lower:]')" &&
    ARCH="$(uname -m | sed -e 's/x86_64/amd64/' -e 's/\(arm\)\(64\)\?.*/\1\2/' -e 's/aarch64$/arm64/')" &&
    KREW="krew-${OS}_${ARCH}" &&
    curl -fsSLO "https://github.com/kubernetes-sigs/krew/releases/latest/download/${KREW}.tar.gz" &&
    tar zxvf "${KREW}.tar.gz" &&
    ./"${KREW}" install krew

kubectl krew install ingress-nginx 
kubectl krew install stern 
kubectl krew install kurt 
kubectl krew install ktop 
kubectl krew install kor 
kubectl krew install cox
kubectl krew install ns
kubectl krew install deprecations
kubectl krew install debug-shell
