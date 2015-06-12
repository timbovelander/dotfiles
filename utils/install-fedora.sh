#!/bin/sh

# Clone dotfiles
sudo dnf install -y git
git clone https://github.com/timbovelander/dotfiles.git "$HOME/.dotfiles"

# Install fonts
sudo dnf install -y google-droid-sans-mono-fonts

# Install ZSH
sudo dnf install -y zsh
sh "$HOME/.dotfiles/utils/install-zsh.sh"

# Install NodeJS
sh "$HOME/.dotfiles/utils/install-node.sh"

# Install ViM
sudo dnf install -y vim-X11 cmake python-devel
sh "$HOME/.dotfiles/utils/install-vim.sh" init

# Install HTML5 Tidy
sh "$HOME/.dotfiles/utils/install-html5tidy.sh"

# Install RVM
sh "$HOME/.dotfiles/utils/install-rvm.sh"

# Copy dotfiles
sudo dnf install -y rsync
rsync -r --exclude-from "$HOME/.dotfiles/utils/exclude-files" \
  "$HOME/.dotfiles/" "$HOME/"
