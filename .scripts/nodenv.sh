#!/usr/bin/env bash

if ! command -v git &>/dev/null; then
  echo "'git' is required to run this script."
  exit
fi

if ! command -v fish &>/dev/null; then
  echo "'fish' is required to run this script."
  exit
fi

if command -v git &>/dev/null && ! command -v nodenv &>/dev/null; then
  if [ -e "$HOME/.nodenv" ]; then
    mv "$HOME/.nodenv" "$HOME/.nodenv.bak"
  fi

  git clone "https://github.com/nodenv/nodenv.git" "$HOME/.nodenv"
  git clone "https://github.com/nodenv/node-build.git" "$HOME/.nodenv/plugins/node-build"
  git clone "https://github.com/nodenv/nodenv-update" "$HOME/.nodenv/plugins/nodenv-update"
  git clone "https://github.com/nodenv/nodenv-default-packages.git" "$HOME/.nodenv/plugins/nodenv-default-packages"

  fish -c 'set -U fish_user_paths $HOME/.nodenv/bin $fish_user_paths'
fi
