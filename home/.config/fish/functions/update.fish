function update
  set -l current_directory (pwd)

  sudo apt-get update
  sudo apt-get dist-upgrade

  npm -g update

  apm upgrade -c false

  cd "$HOME/.dotfiles"
  git pull
  bash "$HOME/.dotfiles/scripts/symlinks.sh"

  cd $current_directory
end
