#!/bin/sh

# Install fonts
sudo dnf install -y google-droid-sans-mono-fonts

# Install Git
sudo dnf install -y git

# Install ZSH
sudo dnf install -y zsh autojump-zsh
wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O - | sh

# Install NodeJS
sudo dnf install -y nodejs npm

# Install ViM
sudo dnf install -y vim-X11

# Clone dotfiles and copy
git clone https://github.com/timbovelander/dotfiles.git $HOME/.dotfiles
rsync -r --exclude-from "$HOME/.dotfiles/exclude-files" $HOME/.dotfiles/ $HOME/

# Install Vundles
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall

# Install RVM
sudo gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
wget https://get.rvm.io -O - | sh -s stable
