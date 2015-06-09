#!/bin/sh

# Clone dotfiles
sudo apt-get install -y git
git clone https://github.com/timbovelander/dotfiles.git "$HOME/.dotfiles"

# Install fonts
sudo apt-get install -y fonts-droid

# Install HTML5 Tidy
sh "$HOME/.dotfiles/utils/install-html5tidy.sh"

# Install ZSH
sudo apt-get install -y zsh
sh "$HOME/.dotfiles/utils/install-zsh.sh"

# Install NodeJS
sudo apt-get install -y nodejs npm
sh "$HOME/.dotfiles/utils/install-node.sh"

# Install ViM
sudo apt-get install -y vim-gtk cmake python-dev
sh "$HOME/.dotfiles/utils/install-vim.sh" init

# Install RVM
sh "$HOME/.dotfiles/utils/install-rvm.sh"

# Copy dotfiles
sudo apt-get install -y rsync
rsync -r --exclude-from "$HOME/.dotfiles/utils/exclude-files" \
  "$HOME/.dotfiles/" "$HOME/"
