# Dotfiles

## TLDR

```shell
./setup.sh [work|personal]
```

## Pre Installation

### WSL 2 (Possibly Outdated)

- Download/Install Ubuntu, VcXsrv, and your favorite font type
- Fix VcXsrv firewall settings to be allowed over public networks
- Make changes to settings\*.json if needed (fonts, image background. etc...)
- Restart

## Post Installation

### General

- Based on your terminal, you may need to set the font accordingly inside the terminal options.
- Launch Docker Desktop if running on Windows or Mac

### Emacs

- install nerd fonts with `M-x nerd-icons-install-fonts`
- init vterm `M-x vterm`

## TODO

- use tree-sitter built in packages to manage installs
- investigate embark use
- look into project.el
- look into using casual suite of tools
- multiple-cursors with completion
