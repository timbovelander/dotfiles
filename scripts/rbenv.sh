#!/usr/bin/env bash

set -u

git clone "https://github.com/rbenv/rbenv.git" "$HOME/.rbenv"
git clone "https://github.com/rbenv/ruby-build.git" "$HOME/.rbenv/plugins/ruby-build"
git clone "https://github.com/jf/rbenv-gemset.git" "$HOME/.rbenv/plugins/rbenv-gemset"

sudo bash "$HOME/.rbenv/plugins/ruby-build/install.sh"
