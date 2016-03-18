#!/usr/bin/env bash

set -u

# install emacs
if ! eval "$packagestatus emacs" 2>/dev/null | eval "$packageinstalled"; then
  eval "$packageinstall emacs"
fi

# remove .emacs and .emacs.d
rm -Rf "$HOME/.emacs" "$HOME/.emacs.d"

# clone spacemacs
git clone https://github.com/syl20bnr/spacemacs.git "$HOME/.emacs.d"
