#!/usr/bin/env bash

init() {
  # Install vundles
  echo "Cloning Vundle to ~/.vim/bundle/Vundle.vim..."
  git clone -q https://github.com/gmarik/Vundle.vim.git "$HOME/.vim/bundle/Vundle.vim"
  echo "Installing and configuring VIM Vundles..."
  vim -u "$HOME/.dotfiles/.vimrc" +PluginInstall -c "!sh $HOME/.dotfiles/utils/install-vim.sh build" +qall
}

build() {
  # Build YouCompleteMe
  mkdir "/tmp/ycm"
  cd "/tmp/ycm"
  cmake -G "Unix Makefiles" . "$HOME/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp"
  make

  # Build vimproc
  cd "$HOME/.vim/bundle/vimproc.vim"
  make

  # Build Vundle tern_for_vim
  cd "$HOME/.vim/bundle/tern_for_vim"
  npm install
}

# Call arguments function
"$@"
