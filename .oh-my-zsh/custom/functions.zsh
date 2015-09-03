ips() {
  dig +short myip.opendns.com @resolver1.opendns.com

  if command -v ip &>/dev/null; then
    ip addr | grep --color=never -oP "inet \K[\d.]+"
  elif command -v ifconfig &>/dev/null; then
    ifconfig | awk "/inet /{ print $2 }"
  fi
}

update() {
  pwd=$(pwd)

  # update distribution
  sudo apt-get update
  sudo apt-get dist-upgrade

  # update vim
  if command -v gvim &>/dev/null; then
    gvim +PluginUpdate +qall
  fi

  # update node modules
  if command -v npm &>/dev/null; then
    sudo npm -g update
    if [ -d "$HOME/.vim/bundle/tern_for_vim" ]; then
      cd "$HOME/.vim/bundle/tern_for_vim"
      npm update
    fi
  fi

  # change back to working directory
  cd $pwd
}
