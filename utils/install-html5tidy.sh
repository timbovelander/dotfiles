#!/bin/bash

git clone https://github.com/htacg/tidy-html5.git /tmp/html5tidy
cd /tmp/html5tidy/build/cmake
cmake ../..
make
sudo make install
rm -Rf /tmp/html5tidy
