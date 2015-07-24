#!/bin/bash

init() {
  # Install vundles
  echo "Cloning Vundle to ~/.vim/bundle/Vundle.vim..."
  git clone -q https://github.com/gmarik/Vundle.vim.git "$HOME/.vim/bundle/Vundle.vim"
  echo "Installing and configuring VIM Vundles..."
  gvim -u "$HOME/.dotfiles/.vimrc" +PluginInstall -c "!sh $HOME/.dotfiles/utils/install-vim.sh build" +qall
}

build() {
  # Build Vundle YouCompleteMe
  mkdir /tmp/ycm_build && cd /tmp/ycm_build
  cmake -G "Unix Makefiles" . ~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp
  make ycm_support_libs

  # Build Vundle tern_for_vim
  cd $HOME/.vim/bundle/tern_for_vim
  npm install
}

# Call arguments function
"$@"
