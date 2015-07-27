#!/usr/bin/env bash

# exit script when a command fails
set -e

# prompt for distribution
echo "Select your linux distribution:"
select distribution in debian cancel
do
  if [ "$distribution" == "cancel" ]; then
    exit 0
  fi
  echo "Starting install for: $distribution"
  break
done

# set variables based on selected distribution
if [ "$distribution" == "debian" ]; then
  packageupdate="sudo apt-get update -y -qq"
  packageinstall="sudo apt-get install -y -qq"
  packageuninstall="sudo apt-get purge -y -qq"
  packagestatus="dpkg-query --status"
  packageinstalled="grep -q 'ok installed'"
  nodesourceurl="https://deb.nodesource.com/setup_0.12"
fi

# update package repository
echo "Updating package repository..."
eval "$packageupdate"

# install git if not yes installed
if ! command -v git &>/dev/null; then
  echo "Installing git..."
  eval "$packageinstall git"
fi

# clone dotfiles repository
echo "Cloning dotfiles to ~/.dotfiles..."
git clone -q https://github.com/timbovelander/dotfiles.git "$HOME/.dotfiles"

# check and install packages
while read package; do
  if ! eval "$packagestatus $package" 2>/dev/null | eval "$packageinstalled"; then
    echo "Installing $package..."
    eval "$packageinstall $package"
  fi
done <"$HOME/.dotfiles/utils/packages-$distribution"

#install fasd
source "$HOME/.dotfiles/utils/install-fasd.sh"

# install node.js
source "$HOME/.dotfiles/utils/install-node.sh"

# install RVM
source "$HOME/.dotfiles/utils/install-rvm.sh"

# install tidy-html5
source "$HOME/.dotfiles/utils/install-html5tidy.sh"

# setup VIM
source "$HOME/.dotfiles/utils/install-vim.sh" init

# install oh-my-zsh
source "$HOME/.dotfiles/utils/install-ohmyzsh.sh"

# copy all dotfiles to home
echo "Copying dotfiles to ~..."
rsync -q -r --exclude-from "$HOME/.dotfiles/utils/exclude-files" \
  "$HOME/.dotfiles/" "$HOME/"
