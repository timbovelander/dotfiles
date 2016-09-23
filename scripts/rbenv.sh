#!/usr/bin/env bash

set -u

if command -v git &>/dev/null; then
  if [ -e "$HOME/.rbenv" ]; then
    mv "$HOME/.rbenv" "$HOME/.rbenv.bak"
  fi

  git clone "https://github.com/rbenv/rbenv.git" "$HOME/.rbenv"
  git clone "https://github.com/rbenv/ruby-build.git" "$HOME/.rbenv/plugins/ruby-build"
  git clone "https://github.com/jf/rbenv-gemset.git" "$HOME/.rbenv/plugins/rbenv-gemset"

  sudo bash "$HOME/.rbenv/plugins/ruby-build/install.sh"
fi
