#!/usr/bin/env bash

set -u

if command -v git &>/dev/null && ! command -v nodenv &>/dev/null; then
  if [ -e "$HOME/.nodenv" ]; then
    mv "$HOME/.nodenv" "$HOME/.nodenv.bak"
  fi

  git clone "https://github.com/nodenv/nodenv.git" "$HOME/.nodenv"
  git clone "https://github.com/nodenv/node-build.git" "$HOME/.nodenv/plugins/node-build"
  git clone "https://github.com/nodenv/nodenv-default-packages.git" "$HOME/.nodenv/plugins/nodenv-default-packages"

  command -v fish &>/dev/null && fish -c 'set -U fish_user_paths $HOME/.nodenv/bin $fish_user_paths'
fi
