#!/bin/bash

init() {
  # Install vundles
  git clone https://github.com/gmarik/Vundle.vim.git "$HOME/.vim/bundle/Vundle.vim"
  gvim -u $HOME/.dotfiles/.vimrc +PluginInstall -c "!sh $HOME/.dotfiles/utils/install-vim.sh build" +qall
}

build() {
  # Build Vundle YouCompleteMe
  mkdir /tmp/ycm_build && cd /tmp/ycm_build
  cmake -G "Unix Makefiles" . ~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp
  make ycm_support_libs
  rm -Rf /tmp/ycm_build

  # Build Vundle tern_for_vim
  cd $HOME/.vim/bundle/tern_for_vim
  npm install
}

# Call arguments function
"$@"
