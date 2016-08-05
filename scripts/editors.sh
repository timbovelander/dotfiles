#!/usr/bin/env bash

set -u

if command -v git &>/dev/null; then
  # emacs
  git clone "https://github.com/syl20bnr/spacemacs" "$HOME/.emacs.d"

  # ViM
  git clone "https://github.com/gmarik/Vundle.vim.git" "$HOME/.vim/bundle/Vundle.vim"
fi
