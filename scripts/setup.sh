#!/usr/bin/env bash

# exit script when a command fails
set -e

# prompt for distribution
echo "Select your linux distribution:"
select distribution in debian opensuse osx cancel
do
  if [ "$distribution" == "cancel" ]; then
    exit 0
  fi
  echo "Starting install for: $distribution"
  break
done

# set variables based on selected distribution
if [ "$distribution" == "debian" ]; then
  packageupdate="sudo apt-get update -y"
  packageinstall="sudo apt-get install -y"
  packageuninstall="sudo apt-get purge -y"
  packagestatus="dpkg-query --status"
  packageinstalled="grep 'ok installed'"
fi
if [ "$distribution" == "opensuse" ]; then
  packageupdate="sudo zypper refresh"
  packageinstall="sudo zypper install -y -l"
  packageuninstall="sudo zypper remove -y"
  packagestatus="rpm -q"
  packageinstalled="grep -v 'not installed'"
fi
if [ "$distribution" == "osx" ]; then
  packageupdate="brew update"
  packageinstall="brew install"
  packageuninstall="brew uninstall --force"
  packagestatus="brew ls --versions"
  packageinstalled="grep ."
fi

# update package repository
eval "$packageupdate"

# install git if not yet installed
if ! command -v git &>/dev/null; then
  eval "$packageinstall git"
fi

# clone dotfiles repository
if [ -e "$HOME/.dotfiles" ]; then
  mv "$HOME/.dotfiles" "$HOME/.dotfiles.bak"
fi
git clone "https://github.com/timbovelander/dotfiles.git" "$HOME/.dotfiles"

# check and install packages
while read package; do
  if ! eval "$packagestatus $package" 2>/dev/null | eval "$packageinstalled"; then
    eval "$packageinstall $package"
  fi
done <"$HOME/.dotfiles/scripts/packages-$distribution"

# change shell
echo "Change shell to fish shell"
chsh -s "/usr/bin/fish"

# install shell plugins
source "$HOME/.dotfiles/scripts/fisherman.sh"

# install fasd
source "$HOME/.dotfiles/scripts/fasd.sh"

# install nodenv
source "$HOME/.dotfiles/scripts/nodenv.sh"

# install rbenv
# source "$HOME/.dotfiles/scripts/rbenv.sh"

# setup editors
source "$HOME/.dotfiles/scripts/editors.sh"

# create symlinks for all home dotfiles
source "$HOME/.dotfiles/scripts/symlinks.sh"
