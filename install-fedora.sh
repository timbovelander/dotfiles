#!/bin/sh

# Elevate rights
if [ $EUID != 0 ]; then
  sudo "$0" "$@"
  exit $?
fi

# Install fonts
dnf install -y google-droid-sans-mono-fonts

# Install Git
dnf install -y git

# Install ZSH
dnf install -y zsh autojump-zsh
wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O - | sh

# Install NodeJS
dnf install -y nodejs npm

# Install ViM
dnf install -y vim-X11
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall

# Clone dotfiles and copy
git clone https://github.com/timbovelander/dotfiles.git $HOME/.dotfiles
rsync -r --exclude-from "$HOME/.dotfiles/exclude-files" $HOME/.dotfiles/ $HOME/

# Install RVM
gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
curl -sSL https://get.rvm.io | sh -s stable
