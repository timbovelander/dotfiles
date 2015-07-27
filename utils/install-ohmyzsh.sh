#!/usr/bin/env bash

set -e

# clone git repository
git clone --depth=1 https://github.com/robbyrussell/oh-my-zsh.git "$HOME/.oh-my-zsh"

# set zsh as default shell
chsh -s /bin/zsh
