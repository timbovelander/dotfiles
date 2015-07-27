#!/usr/bin/env bash

if ! command -v rvm &>/dev/null; then
  globalgems="scss_lint"

  # Add key
  gpg2 --keyserver hkp://keys.gnupg.net --recv-keys \
  409B6B1796C275462A1703113804BB82D39DC0E3

  # Install RVM
  echo "Installing RVM..."
  curl -sSL https://get.rvm.io | bash -s stable --ruby \
  --with-gems="$globalgems"
fi
