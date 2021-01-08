# dotfiles

## Wsl2
At this point only 5 steps needed. 
* Download/Install Ubuntu, VcXsrv, and your favorite font type
* If using Hack then install from Nerd Fonts!
* Fix VcXsrv firewall settings to be allowed over public networks
* Make changes to settings*.json if needed (fonts, image background. etc...)
* Create/Add your github rsa key to ~/.ssh/github and link it to github
* Restart

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
### TODO: fix Batcat for 18.04 >=
for Ubuntu 18.04 >=
export VER="0.17.1"
wget "https://github.com/sharkdp/bat/releases/download/v${VER}/bat_${VER}_amd64.deb"
sudo dpkg -i bat_${VER}_amd64.deb
