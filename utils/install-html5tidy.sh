#!/usr/bin/env bash

# exit when variables are not set
set -u

# uninstall default tidy
if eval "$packagestatus tidy" 2>/dev/null | eval "$packageinstalled"; then
  echo "Uninstalling tidy..."
  eval "$packageuninstall tidy"
fi

# install tidy
if ! command -v tidy &>/dev/null; then
  echo "Cloning tidy-html5 to /tmp..."
  git clone -q https://github.com/htacg/tidy-html5.git /tmp/html5tidy
  cd /tmp/html5tidy/build/cmake

  echo "Building tidy-html5..."
  cmake ../..
  make

  echo "Installing tidy-html5..."
  sudo make install
fi
