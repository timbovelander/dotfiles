#!/usr/bin/env bash

if [ -e "$HOME/.dotfiles" ]; then
  mv "$HOME/.dotfiles" "$HOME/.dotfiles.bak"
fi

git clone --bare "https://github.com/timbovelander/dotfiles.git" "$HOME/.dotfiles"
git --git-dir="$HOME/.dotfiles" --work-tree="$HOME" checkout -f