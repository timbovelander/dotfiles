#!/usr/bin/env bash

# create all directories
find "$HOME/.dotfiles/home" -type d | sed -r 's/(.*)\/\.dotfiles\/home/\1/' | xargs mkdir -p

# create symlinks for all files, backup old files
while read src; do
  target=$(echo $src | sed -r 's/(.*)\/\.dotfiles\/home/\1/')
  ln -sfb "$src" "$target"
done < <(exec find "$HOME/.dotfiles/home" -type f)
