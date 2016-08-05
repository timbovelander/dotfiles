#!/usr/bin/env bash

if ! command -v fasd &>/dev/null; then
  wget -q -O /tmp/fasd.tar.gz https://github.com/clvv/fasd/tarball/1.0.1
  cd "/tmp"
  tar -xzf /tmp/fasd.tar.gz --transform="s|^.*/|fasd/|"
  cd "/tmp/fasd"
  sudo make install
fi
