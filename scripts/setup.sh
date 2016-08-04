#!/usr/bin/env bash

# exit script when a command fails
set -e

# prompt for distribution
echo "Select your linux distribution:"
select distribution in debian opensuse cancel
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

# update package repository
echo "Updating package repository..."
eval "$packageupdate"

# install git if not yet installed
if ! command -v git &>/dev/null; then
  echo "Installing git..."
  eval "$packageinstall git"
fi

# clone dotfiles repository
echo "Cloning dotfiles to ~/.dotfiles..."
git clone "https://github.com/timbovelander/dotfiles.git" "$HOME/.dotfiles"

# check and install packages
while read package; do
  if ! eval "$packagestatus $package" 2>/dev/null | eval "$packageinstalled"; then
    echo "Installing $package..."
    eval "$packageinstall $package"
  fi
done <"$HOME/.dotfiles/scripts/packages-$distribution"

# change shell
chsh -s "/usr/bin/fish"

# install fasd
source "$HOME/.dotfiles/scripts/fasd.sh"

# install rbenv
source "$HOME/.dotfiles/scripts/rbenv.sh"

# install editors
source "$HOME/.dotfiles/scripts/editors.sh"

# create symlinks for all home dotfiles
source "$HOME/.dotfiles/scripts/symlinks.sh"
