# Dotfiles

An opinionated configuration for a \*nix machines. The goal of this is like any dotfiles repo, provide a version controlled repo to manage everything I use on a daily basis. This helps me track bugs, features, learned lessons, etc all while providing a simpler way to setup new machines with what I need. The setup could be improved in a multitude of ways especially when it comes to organization, speed, extensibility, and ease of use as well as a plethora of other things but for now it works and I will continue to chip away at it when I got time.

I am hoping this repo can be useful as a basis for others to manage dotfiles. If I did something egregious, let me know and I don't mind addressing it.

## TLDR

YOLO

```shell
./setup.sh -e [work|personal]
```

For a detailed help message

```shell
./setup.sh -h
```

## Pre Installation

### WSL 2 (Possibly Outdated)

- Download/Install Ubuntu, VcXsrv, and your favorite font type
- Fix VcXsrv firewall settings to be allowed over public networks
- Make changes to settings\*.json if needed (fonts, image background. etc...)

## Post Installation

### General

- Based on your terminal, you may need to set the font accordingly inside the terminal options.
- Launch Docker Desktop if running on Windows or Macs

### Emacs

- install nerd fonts with `M-x nerd-icons-install-fonts`
- init vterm `M-x vterm`

## Top Level Directory Structure

```shell
.
|-- README.md
|-- bootstrap.sh
|-- bootstrap_extensions
|-- clean.sh
|-- emacs
|-- fonts
|-- package_managers
|-- setup.sh
|-- utils.sh
|-- wsl
`-- zsh
```

- `setup.sh`: Script to run to setup the machine. Use `-h` for more information on the arguments possible to pass to it. Contains extensive
- `bootstrap.sh`: Script that orchestrates the installation and symlinking of all packages and files respectively.
- `utils.sh`: Script containing reusable functions to facilitate prompt creation and user input.
- `clean.sh`: Misleading script since it does not really do a good job at cleaning up everything installed from `setup.sh` but it does whats been helpful for testing.
- `bootstrap_extensions`: Directory that contains scripts, each with a purpose of being as self contained as possible. Adding another language configuration is as simple as adding the script with the setup steps needed in it.
- `emacs`: My glorious Emacs config. Marvel at how much I waste my life on it... Its a part-time job
- `fonts`: fonts I use so I no longer need to fetch them... Ran into too many issues trying to install them via multiple package managers.
- `package_managers`: Package managers package installation lists. This contains the basis for extensions although extensions should ideally run this themselves to be truly self contained.
- `wsl`: I use WSL 2 on my windows machines and this provides some common configuration I use to get things working. These files are used by `./bootstrap_extensions/wsl_bootstrap.sh`
- `zsh`: Files for base `ZSH` shell setup with `p10k`. This majority of files in this folder are symlinked to the `$HOME` folder. `OMZ` plugin installation is provided by `./bootstrap_extensions/zsh_bootstrap.sh`.

## TODO

- Yaml
  - yaml mode tab for complete command
- Helm
  - make editing helm files nicer
- Markdown
  - adding markdown-ts support to markdown mode
- General
  - investigate embark, vertico, avy
  - look into using casual suite of tools
  - corg.el
- Web mode
  - replacing web-mode functionality (web-mode-element-next, previous, element-wrap)
  - html mode s-h keybinding
  - better treesit support for angular
