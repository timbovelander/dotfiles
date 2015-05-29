# Update system
update() {
  # Update fedora
  sudo dnf update

  # Update oh_my_zsh
  upgrade_oh_my_zsh

  # Update RVM & global gems
  rvm get stable
  rvm @global do gem update

  # Update global Node modules
  sudo npm update -g
}
