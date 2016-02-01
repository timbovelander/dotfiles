function update
  set -l current_directory (pwd)

  sudo apt-get update
  sudo apt-get dist-upgrade

  vim +PluginUpdate +qall

  cd "$HOME/.vim/bundle/vimproc.vim"
  make

  cd "$HOME/.vim/bundle/YouCompleteMe/third_party/ycmd/third_party/tern"
  npm update

  sudo npm -g update

  cd $current_directory
end
