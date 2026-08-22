#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail

script_directory="$(
  cd -- "$(dirname -- "${BASH_SOURCE[0]}")"
  pwd
)"
repository_root="$(
  cd -- "${script_directory}/.."
  pwd
)"

# shellcheck source=../utils.sh
source "${repository_root}/utils.sh"

PROMPT='[ AptInstaller ]: '

: "${KUBERNETES_VERSION:?KUBERNETES_VERSION is required}"
kubernetes_repository_version="v${KUBERNETES_VERSION}"

if (( EUID != 0 )); then
  echo_with_red_prompt "This script must run as root"
  exit 1
fi

if [[ ! -r /etc/os-release ]]; then
  echo_with_red_prompt "Unable to determine the Linux distribution"
  exit 1
fi

# shellcheck source=/etc/os-release
source /etc/os-release

if [[ "${ID}" != 'ubuntu' ]]; then
  echo_with_red_prompt \
    "This installer currently supports Ubuntu, not ${ID}"
  exit 1
fi

export DEBIAN_FRONTEND=noninteractive

apt_architecture="$(dpkg --print-architecture)"

case "${apt_architecture}" in
  amd64)
    release_architecture='amd64'
    aws_architecture='x86_64'
    hadolint_architecture='x86_64'
    lazydocker_architecture='x86_64'
    ;;
  arm64)
    release_architecture='arm64'
    aws_architecture='aarch64'
    hadolint_architecture='arm64'
    lazydocker_architecture='arm64'
    ;;
  *)
    echo_with_red_prompt \
      "Unsupported architecture: ${apt_architecture}"
    exit 1
    ;;
esac

install_apt_packages() {
  local packages=("$@")

  apt-get install \
    --yes \
    --no-install-recommends \
    "${packages[@]}"
}

github_latest_tag() {
  local repository="${1:?missing GitHub repository}"

  curl \
    --fail \
    --silent \
    --show-error \
    --location \
    "https://api.github.com/repos/${repository}/releases/latest" |
    jq --raw-output '.tag_name'
}

install_base_packages() {
  local packages=(
    apt-transport-https
    autoconf
    automake
    bat
    build-essential
    ca-certificates
    ccls
    cmake
    coreutils
    curl
    dbus-x11
    dnsutils
    enchant-2
    fd-find
    fzf
    gdb
    git
    git-lfs
    gnupg
    htop
    imagemagick
    jq
    keychain
    libbz2-dev
    libenchant-2-dev
    libffi-dev
    libjansson-dev
    liblzma-dev
    libncurses-dev
    libpng-dev
    libpoppler-glib-dev
    libpq-dev
    librdkafka-dev
    libreadline-dev
    libsqlite3-dev
    libssl-dev
    libtool
    libtool-bin
    libxml2-dev
    libxmlsec1-dev
    llvm
    make
    mtr
    net-tools
    nmap
    opensc
    pandoc
    pgformatter
    pkg-config
    plantuml
    poppler-utils
    postgresql
    postgresql-client
    qutebrowser
    ripgrep
    shellcheck
    software-properties-common
    strace
    texlive-fonts-extra
    texlive-latex-base
    texlive-latex-extra
    texlive-latex-recommended
    tk-dev
    tmux
    tree
    universal-ctags
    unzip
    wget
    xclip
    xz-utils
    yamllint
    zip
    zlib1g-dev
    zsh
  )

  install_apt_packages "${packages[@]}"

  if apt-cache show 7zip >/dev/null 2>&1; then
    install_apt_packages 7zip
  else
    install_apt_packages p7zip-full
  fi
}

configure_command_aliases() {
  if ! command -v fd >/dev/null 2>&1 &&
     [[ -x /usr/bin/fdfind ]]; then
    ln -s /usr/bin/fdfind /usr/local/bin/fd
  fi

  if ! command -v bat >/dev/null 2>&1 &&
     [[ -x /usr/bin/batcat ]]; then
    ln -s /usr/bin/batcat /usr/local/bin/bat
  fi
}

configure_docker_repository() {
  install -m 0755 -d /etc/apt/keyrings

  curl \
    --fail \
    --silent \
    --show-error \
    --location \
    https://download.docker.com/linux/ubuntu/gpg \
    -o /etc/apt/keyrings/docker.asc

  chmod a+r /etc/apt/keyrings/docker.asc

  cat > /etc/apt/sources.list.d/docker.sources <<EOF
Types: deb
URIs: https://download.docker.com/linux/ubuntu
Suites: ${UBUNTU_CODENAME:-$VERSION_CODENAME}
Components: stable
Architectures: ${apt_architecture}
Signed-By: /etc/apt/keyrings/docker.asc
EOF
}

configure_github_cli_repository() {
  install -m 0755 -d \
    /etc/apt/keyrings \
    /etc/apt/sources.list.d

  curl \
    --fail \
    --silent \
    --show-error \
    --location \
    https://cli.github.com/packages/githubcli-archive-keyring.gpg \
    -o /etc/apt/keyrings/githubcli-archive-keyring.gpg

  chmod go+r \
    /etc/apt/keyrings/githubcli-archive-keyring.gpg

  cat > /etc/apt/sources.list.d/github-cli.list <<EOF
deb [arch=${apt_architecture} signed-by=/etc/apt/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main
EOF
}

configure_helm_repository() {
  local expected_fingerprint
  local actual_fingerprint
  local temporary_key

  expected_fingerprint='DDF78C3E6EBB2D2CC223C95C62BA89D07698DBC6'
  temporary_key="$(mktemp)"

  curl \
    --fail \
    --silent \
    --show-error \
    --location \
    https://packages.buildkite.com/helm-linux/helm-debian/gpgkey \
    -o "${temporary_key}"

  actual_fingerprint="$(
    gpg \
      --show-keys \
      --with-colons \
      "${temporary_key}" |
      awk -F: '$1 == "fpr" { print $10; exit }'
  )"

  if [[ "${actual_fingerprint}" != "${expected_fingerprint}" ]]; then
    rm -f "${temporary_key}"
    echo_with_red_prompt \
      "Unexpected Helm repository key fingerprint"
    return 1
  fi

  gpg \
    --dearmor \
    --yes \
    --output /usr/share/keyrings/helm.gpg \
    "${temporary_key}"

  rm -f "${temporary_key}"

  cat > /etc/apt/sources.list.d/helm.list <<EOF
deb [signed-by=/usr/share/keyrings/helm.gpg] https://packages.buildkite.com/helm-linux/helm-debian/any/ any main
EOF
}

configure_kubernetes_repository() {
  install -m 0755 -d /etc/apt/keyrings

  curl \
    --fail \
    --silent \
    --show-error \
    --location \
    "https://pkgs.k8s.io/core:/stable:/${kubernetes_repository_version}/deb/Release.key" |
    gpg \
      --dearmor \
      --yes \
      --output /etc/apt/keyrings/kubernetes-apt-keyring.gpg

  cat > /etc/apt/sources.list.d/kubernetes.list <<EOF
deb [signed-by=/etc/apt/keyrings/kubernetes-apt-keyring.gpg] https://pkgs.k8s.io/core:/stable:/${kubernetes_repository_version}/deb/ /
EOF
}

configure_opentofu_repository() {
  install -m 0755 -d /etc/apt/keyrings

  curl \
    --fail \
    --silent \
    --show-error \
    --location \
    https://get.opentofu.org/opentofu.gpg \
    -o /etc/apt/keyrings/opentofu.gpg

  curl \
    --fail \
    --silent \
    --show-error \
    --location \
    https://packages.opentofu.org/opentofu/tofu/gpgkey |
    gpg \
      --no-tty \
      --batch \
      --dearmor \
      --yes \
      --output /etc/apt/keyrings/opentofu-repo.gpg

  chmod a+r \
    /etc/apt/keyrings/opentofu.gpg \
    /etc/apt/keyrings/opentofu-repo.gpg

  cat > /etc/apt/sources.list.d/opentofu.list <<EOF
deb [signed-by=/etc/apt/keyrings/opentofu.gpg,/etc/apt/keyrings/opentofu-repo.gpg] https://packages.opentofu.org/opentofu/tofu/any/ any main
EOF
}

configure_trivy_repository() {
  curl \
    --fail \
    --silent \
    --show-error \
    --location \
    https://aquasecurity.github.io/trivy-repo/deb/public.key |
    gpg \
      --dearmor \
      --yes \
      --output /usr/share/keyrings/trivy.gpg

  cat > /etc/apt/sources.list.d/trivy.list <<EOF
deb [signed-by=/usr/share/keyrings/trivy.gpg] https://aquasecurity.github.io/trivy-repo/deb generic main
EOF
}

install_repository_packages() {
  apt-get update

  install_apt_packages \
    containerd.io \
    docker-buildx-plugin \
    docker-ce \
    docker-ce-cli \
    docker-compose-plugin \
    gh \
    helm \
    kubectl \
    tofu \
    trivy

  # Docker normally starts automatically. This condition also permits
  # installation in WSL distributions that do not run systemd.
  if command -v systemctl >/dev/null 2>&1 &&
     systemctl is-system-running >/dev/null 2>&1; then
    systemctl enable --now docker
  fi

  # Membership in the Docker group grants root-equivalent access.
  if [[ -n "${SUDO_USER:-}" ]] &&
     [[ "${SUDO_USER}" != 'root' ]]; then
    usermod -aG docker "${SUDO_USER}"
  fi
}

install_aws_cli() {
  local archive
  local temporary_directory

  archive="awscli-exe-linux-${aws_architecture}.zip"
  temporary_directory="$(mktemp -d)"

  curl \
    --fail \
    --silent \
    --show-error \
    --location \
    "https://awscli.amazonaws.com/${archive}" \
    -o "${temporary_directory}/${archive}"

  unzip \
    -q \
    "${temporary_directory}/${archive}" \
    -d "${temporary_directory}"

  if command -v aws >/dev/null 2>&1; then
    "${temporary_directory}/aws/install" \
      --update \
      --install-dir /usr/local/aws-cli \
      --bin-dir /usr/local/bin
  else
    "${temporary_directory}/aws/install" \
      --install-dir /usr/local/aws-cli \
      --bin-dir /usr/local/bin
  fi

  rm -r "${temporary_directory}"
}

install_argocd() {
  local temporary_file

  temporary_file="$(mktemp)"

  curl \
    --fail \
    --silent \
    --show-error \
    --location \
    "https://github.com/argoproj/argo-cd/releases/latest/download/argocd-linux-${release_architecture}" \
    -o "${temporary_file}"

  install \
    -o root \
    -g root \
    -m 0755 \
    "${temporary_file}" \
    /usr/local/bin/argocd

  rm -f "${temporary_file}"
}

install_hadolint() {
  local temporary_file

  temporary_file="$(mktemp)"

  curl \
    --fail \
    --silent \
    --show-error \
    --location \
    "https://github.com/hadolint/hadolint/releases/latest/download/hadolint-Linux-${hadolint_architecture}" \
    -o "${temporary_file}"

  install \
    -o root \
    -g root \
    -m 0755 \
    "${temporary_file}" \
    /usr/local/bin/hadolint

  rm -f "${temporary_file}"
}

install_kubeseal() {
  local tag
  local version
  local temporary_directory
  local archive

  tag="$(github_latest_tag bitnami-labs/sealed-secrets)"
  version="${tag#v}"
  archive="kubeseal-${version}-linux-${release_architecture}.tar.gz"
  temporary_directory="$(mktemp -d)"

  curl \
    --fail \
    --silent \
    --show-error \
    --location \
    "https://github.com/bitnami-labs/sealed-secrets/releases/download/${tag}/${archive}" \
    -o "${temporary_directory}/${archive}"

  tar \
    -xzf "${temporary_directory}/${archive}" \
    -C "${temporary_directory}" \
    kubeseal

  install \
    -o root \
    -g root \
    -m 0755 \
    "${temporary_directory}/kubeseal" \
    /usr/local/bin/kubeseal

  rm -r "${temporary_directory}"
}

install_kustomize() {
  local tag
  local encoded_tag
  local version
  local archive
  local temporary_directory

  tag="$(github_latest_tag kubernetes-sigs/kustomize)"
  encoded_tag="${tag//\//%2F}"
  version="${tag#kustomize/}"
  archive="kustomize_${version}_linux_${release_architecture}.tar.gz"
  temporary_directory="$(mktemp -d)"

  curl \
    --fail \
    --silent \
    --show-error \
    --location \
    "https://github.com/kubernetes-sigs/kustomize/releases/download/${encoded_tag}/${archive}" \
    -o "${temporary_directory}/${archive}"

  tar \
    -xzf "${temporary_directory}/${archive}" \
    -C "${temporary_directory}" \
    kustomize

  install \
    -o root \
    -g root \
    -m 0755 \
    "${temporary_directory}/kustomize" \
    /usr/local/bin/kustomize

  rm -r "${temporary_directory}"
}

install_lazydocker() {
  local tag
  local version
  local archive
  local temporary_directory

  tag="$(github_latest_tag jesseduffield/lazydocker)"
  version="${tag#v}"
  archive="lazydocker_${version}_Linux_${lazydocker_architecture}.tar.gz"
  temporary_directory="$(mktemp -d)"

  curl \
    --fail \
    --silent \
    --show-error \
    --location \
    "https://github.com/jesseduffield/lazydocker/releases/download/${tag}/${archive}" \
    -o "${temporary_directory}/${archive}"

  tar \
    -xzf "${temporary_directory}/${archive}" \
    -C "${temporary_directory}" \
    lazydocker

  install \
    -o root \
    -g root \
    -m 0755 \
    "${temporary_directory}/lazydocker" \
    /usr/local/bin/lazydocker

  rm -r "${temporary_directory}"
}

install_minikube() {
  local temporary_file

  temporary_file="$(mktemp --suffix=.deb)"

  curl \
    --fail \
    --silent \
    --show-error \
    --location \
    "https://storage.googleapis.com/minikube/releases/latest/minikube_latest_${release_architecture}.deb" \
    -o "${temporary_file}"

  apt-get install \
    --yes \
    "${temporary_file}"

  rm -f "${temporary_file}"
}

install_yq() {
  local temporary_file

  temporary_file="$(mktemp)"

  curl \
    --fail \
    --silent \
    --show-error \
    --location \
    "https://github.com/mikefarah/yq/releases/latest/download/yq_linux_${release_architecture}" \
    -o "${temporary_file}"

  install \
    -o root \
    -g root \
    -m 0755 \
    "${temporary_file}" \
    /usr/local/bin/yq

  rm -f "${temporary_file}"
}

verify_installation() {
  local commands=(
    argocd
    aws
    bat
    ccls
    cmake
    docker
    fd
    fzf
    gh
    git
    hadolint
    helm
    jq
    kubectl
    kubeseal
    kustomize
    lazydocker
    minikube
    nmap
    pandoc
    pg_format
    plantuml
    rg
    shellcheck
    tofu
    trivy
    yq
    yamllint
  )
  local missing=()
  local command_name

  for command_name in "${commands[@]}"; do
    if ! command -v "${command_name}" >/dev/null 2>&1; then
      missing+=("${command_name}")
    fi
  done

  if (( ${#missing[@]} > 0 )); then
    echo_with_red_prompt \
      "Missing commands: ${missing[*]}"
    return 1
  fi

  echo_with_green_prompt \
    "All expected Linux tools are installed"
}

main() {
  echo_with_prompt "Updating APT metadata"
  apt-get update

  echo_with_prompt "Installing Ubuntu packages"
  install_base_packages

  echo_with_prompt "Configuring command aliases"
  configure_command_aliases

  echo_with_prompt "Configuring Docker repository"
  configure_docker_repository

  echo_with_prompt "Configuring GitHub CLI repository"
  configure_github_cli_repository

  echo_with_prompt "Configuring Helm repository"
  configure_helm_repository

  echo_with_prompt "Configuring Kubernetes repository"
  configure_kubernetes_repository

  echo_with_prompt "Configuring OpenTofu repository"
  configure_opentofu_repository

  echo_with_prompt "Configuring Trivy repository"
  configure_trivy_repository

  echo_with_prompt "Installing repository packages"
  install_repository_packages

  echo_with_prompt "Installing AWS CLI"
  install_aws_cli

  echo_with_prompt "Installing Argo CD"
  install_argocd

  echo_with_prompt "Installing Hadolint"
  install_hadolint

  echo_with_prompt "Installing Kubeseal"
  install_kubeseal

  echo_with_prompt "Installing Kustomize"
  install_kustomize

  echo_with_prompt "Installing Lazydocker"
  install_lazydocker

  echo_with_prompt "Installing Minikube"
  install_minikube

  echo_with_prompt "Installing yq"
  install_yq

  echo_with_prompt "Verifying installed tools"
  verify_installation

  echo_with_green_prompt \
    "APT package installation complete"
}

main "$@"
