#!/bin/sh

# Clone dotfiles
git clone https://github.com/timbovelander/dotfiles.git "$HOME/.dotfiles"

# Install fonts
sudo dnf install -y google-droid-sans-mono-fonts

# Install install dependencies
sudo dnf install -y git rsync cmake python-devel

# Install ZSH
sudo dnf install -y zsh autojump-zsh
sh "$HOME/.dotfiles/scripts/install-zsh.sh"

# Install NodeJS
sudo dnf install -y nodejs npm
sh "$HOME/.dotfiles/scripts/install-node.sh"

# Install ViM
sudo dnf install -y vim-X11 vim-enhanced
sh "$HOME/.dotfiles/scripts/install-vim.sh"

# Install RVM
sh "$HOME/.dotfiles/scripts/install-rvm.sh"

# Copy dotfiles
rsync -r --exclude-from "$HOME/.dotfiles/utils/exclude-files" $HOME/.dotfiles/ $HOME/
