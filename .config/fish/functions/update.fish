function update
  sudo apt-get update
  sudo apt-get dist-upgrade

  vim +PluginUpdate +qall

  cd "$HOME/.vim/bundle/vimproc.vim"
  make

  cd "$HOME/.vim/bundle/tern_for_vim"
  npm update

  sudo npm -g update
end
