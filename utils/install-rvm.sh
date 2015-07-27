#!/usr/bin/env bash

if ! command -v rvm &>/dev/null; then
  # Add key
  gpg2 --keyserver hkp://keys.gnupg.net --recv-keys \
  409B6B1796C275462A1703113804BB82D39DC0E3 >/dev/null

  # Install RVM
  echo "Installing RVM..."
  curl -sSL https://get.rvm.io | bash -s stable --ruby

  # Source RVM
  source "$HOME/.rvm/scripts/rvm"
fi

# Install global gems
if command -v rvm &>/dev/null; then
  while read gem; do
    if ! rvm @global do gem list -i $gem >/dev/null; then

      echo "Installing global gem $gem..."
      rvm @global do gem install $gem >/dev/null

    fi
  done <"$HOME/.dotfiles/utils/ruby-gems"
fi
