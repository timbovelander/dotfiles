#!/usr/bin/env bash

set -u

if command -v fish &>/dev/null; then
  curl "https://git.io/fisher" --create-dirs -sLo "$HOME/.config/fish/functions/fisher.fish"
  fish -c "fisher"
fi
