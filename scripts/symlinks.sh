#!/usr/bin/env bash

# create all directories
find "$HOME/.dotfiles/home" -type d | sed 's/\.dotfiles\/home\///' | xargs mkdir -p

# create symlinks for all files, backup old files
while read src; do
  target=$(echo $src | sed 's/\.dotfiles\/home\///')
  mv "$target" "$target.bak"
  ln -sf "$src" "$target"
done < <(exec find "$HOME/.dotfiles/home" -type f)
