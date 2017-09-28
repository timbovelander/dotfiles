#!/usr/bin/env bash

set -u

if command -v git &>/dev/null; then
  # emacs
  if [ -e "$HOME/.emacs" ]; then
    mv "$HOME/.emacs" "$HOME/.emacs.bak"
  fi
  if [ -e "$HOME/.emacs.d" ]; then
    mv "$HOME/.emacs.d" "$HOME/.emacs.d.bak"
  fi
  git clone "https://github.com/syl20bnr/spacemacs" "$HOME/.emacs.d"

  # ViM
  if [ -e "$HOME/.vim/bundle/Vundle.vim" ]; then
    mv "$HOME/.vim/bundle/Vundle.vim" "$HOME/.vim/bundle/Vundle.vim.bak"
  fi
  git clone "https://github.com/gmarik/Vundle.vim.git" "$HOME/.vim/bundle/Vundle.vim"

  # atom
  if command -v apm &>/dev/null; then
    apm install --packages-file "$HOME/.dotfiles/home/.atom/packages.list"
  fi
fi
