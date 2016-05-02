function update
  set -l current_directory (pwd)

  sudo apt-get update
  sudo apt-get dist-upgrade

  npm -g update

  cd "$HOME/.dotfiles"
  git pull
  bash "$HOME/.dotfiles/scripts/symlinks.sh"

  cd "$HOME/.emacs.d"
  git pull

  cd $current_directory
end
