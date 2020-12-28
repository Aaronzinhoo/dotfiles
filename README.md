# dotfiles

## Wsl2
At this point only 5 steps needed. 
* Download/Install Ubuntu, VcXsrv, and your favorite font type (I use Hack)
* Fix VcXsrv firewall settings to be allowed over public networks
* Make changes to settings*.json if needed (fonts, image background. etc...)
* Create/Add your github rsa key to ~/.ssh/github and link it to github
* Restart Your Computer

## Arguments
When running `bootstrap.sh` there is only 1 argument you can feed and that is `work`:
- work: used to install work setup. If not, then a more robust "personal" setup will install
