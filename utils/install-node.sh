#!/usr/bin/env bash

# exit when variables are not set
set -u

# install nodejs
if ! eval "$packagestatus nodejs" 2>/dev/null | eval "$packageinstalled"; then
  echo "Installing nodejs..."
  curl -sL "$nodesourceurl" | sudo bash - >/dev/null
  eval "$packageinstall nodejs"
fi

# install global Node modules
if command -v npm &>/dev/null; then
  while read module; do
    if ! sudo npm -g list $module --depth=0 &>/dev/null; then

      echo "Installing global node module $module..."
      sudo npm -g install $module >/dev/null

    fi
  done <"$HOME/.dotfiles/utils/node-modules"
fi
