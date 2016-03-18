#!/usr/bin/env bash

set -u

init() {
  # Install vim
  if ! eval "$packagestatus vim" 2>/dev/null | eval "$packageinstalled"; then
    eval "$packageinstall vim"
  fi

  # Install vundles
  echo "Cloning Vundle to ~/.vim/bundle/Vundle.vim..."
  git clone -q https://github.com/gmarik/Vundle.vim.git "$HOME/.vim/bundle/Vundle.vim"
  echo "Installing and configuring VIM Vundles..."
  vim -u "$HOME/.dotfiles/.vimrc" +PluginInstall -c "!sh $HOME/.dotfiles/scripts/vim.sh build" +qall
}

build() {
  # Build vimproc
  cd "$HOME/.vim/bundle/vimproc.vim"
  make
}

# Call arguments function
"$@"
