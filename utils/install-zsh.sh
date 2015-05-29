#!/bin/sh

# Change default shell to zsh
sudo chsh $USER -s /bin/zsh

# Install oh-my-zsh
wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O - | sh
