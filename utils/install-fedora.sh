#!/bin/sh

# Clone dotfiles
sudo dnf install git
git clone https://github.com/timbovelander/dotfiles.git "$HOME/.dotfiles"

# Install fonts
sudo dnf install -y google-droid-sans-mono-fonts

# Install ZSH
sudo dnf install -y zsh
sh "$HOME/.dotfiles/utils/install-zsh.sh"

# Install NodeJS
sudo dnf install -y nodejs npm
sh "$HOME/.dotfiles/utils/install-node.sh"

# Install ViM
sudo dnf install -y vim-X11 cmake python-devel
sh "$HOME/.dotfiles/utils/install-vim.sh" init

# Install RVM
sh "$HOME/.dotfiles/utils/install-rvm.sh"

# Copy dotfiles
sudo dnf install rsync
rsync -r --exclude-from "$HOME/.dotfiles/utils/exclude-files" $HOME/.dotfiles/ $HOME/
