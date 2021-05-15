# dotfiles

## Wsl2

At this point only 5 steps needed.

- Download/Install Ubuntu, VcXsrv, and your favorite font type
- Fix VcXsrv firewall settings to be allowed over public networks
- Make changes to settings\*.json if needed (fonts, image background. etc...)
- Create/Add your github rsa key to ~/.ssh/github and link it to github
- Restart

## Arguments

When running `install.sh` there is only 1 argument you can feed and that is `work`:

- work: used to install work setup. If not, then a more robust "personal" setup will install

### Extra Changes

Python uses a hard coded 3.8.6 version which can be adjusted by hand if needed. NVM installs the LTS version of node

## TODO

### eshell completion

- find ways to make eshell faster and easier to use

### org-agenda & time-tracking

- setup time tracking and task organization with org
