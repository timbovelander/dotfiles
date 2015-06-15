#!/bin/bash

# Clone bash-it
git clone --depth=1 https://github.com/Bash-it/bash-it.git $HOME/.bash_it

# Source .bashrc
source $HOME/.dotfiles/.bashrc

# Enable aliases
bash-it enable alias clipboard
bash-it enable alias git
bash-it enable alias vim

# Enable completions
bash-it enable completion bash-it
bash-it enable completion git
bash-it enable completion grunt
bash-it enable completion gulp
bash-it enable completion npm
bash-it enable completion ssh

# Enable plugins
bash-it enable plugin base
bash-it enable plugin dirs
bash-it enable plugin rvm
