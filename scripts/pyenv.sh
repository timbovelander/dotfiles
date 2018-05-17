#!/usr/bin/env bash

set -u

if command -v git &>/dev/null && ! command -v pyenv &>/dev/null; then
  if [ -e "$HOME/.pyenv" ]; then
    mv "$HOME/.pyenv" "$HOME/.pyenv.bak"
  fi

  git clone "https://github.com/pyenv/pyenv.git" "$HOME/.pyenv"

  command -v fish &>/dev/null && fish -c 'set -U PYENV_ROOT $HOME/.pyenv'
  command -v fish &>/dev/null && fish -c 'set -U fish_user_paths $PYENV_ROOT/bin $fish_user_paths'
fi
