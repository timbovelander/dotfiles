#!/bin/sh

# Install Vundles
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
gvim -u $HOME/.dotfiles/.vimrc_vundle +PluginInstall +qall

# Build Vundle YouCompleteMe
mkdir /tmp/ycm_build && cd /tmp/ycm_build
cmake -G "Unix Makefiles" . ~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp
make ycm_support_libs

# Build Vundle tern_for_vim
cd $HOME/.vim/bundle/tern_for_vim
npm install
