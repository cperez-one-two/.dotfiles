#
# ~/.bash_profile - sources shell-agnostic profile and bashrc for bash
#

[ -f ~/.config/shell/envs ] && . ~/.config/shell/envs
[ -f ~/.config/bash/bashrc ] && . ~/.config/bash/bashrc

if [ -e /home/uhoh/.nix-profile/etc/profile.d/nix.sh ]; then . /home/uhoh/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
