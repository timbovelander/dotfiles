#!/usr/bin/env bash

set -e

echo "Copying dotfiles to ~..."
rsync -q -r --exclude-from "$HOME/.dotfiles/utils/exclude-files" \
  "$HOME/.dotfiles/" "$HOME/"
