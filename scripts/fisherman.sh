#!/usr/bin/env bash

set -u

if command -v fish &>/dev/null; then
  curl -Lo "$HOME/.config/fish/functions/fisher.fish" --create-dirs "git.io/fisher"
  fish -c "fisher"
fi
