# ===== Functions

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

# Pygmentize a file
function ccat() {
  printf '\n\n'
  pygmentize "$1"
  printf '\n'
}

# Open with nautilus
function o() {
  if (($# == 0))
    then
      nautilus .
    else
      nautilus "$1"
  fi
}

# Open with gvim
function v() {
  if (($# == 0))
    then
      gvim .
    else
      gvim "$1"
  fi
}
