#!/bin/sh

# Add key
gpg2 --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3

# Install RVM
wget https://get.rvm.io -O - | bash -s stable --ruby

# Source RVM
source "$HOME/.profile"

# Install global gems
rvm @global do gem install scss_lint
