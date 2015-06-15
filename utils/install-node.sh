#!/bin/bash

# Download NodeJS
mkdir /tmp/nodejs && cd /tmp/nodejs
wget http://nodejs.org/dist/v0.12.4/node-v0.12.4-linux-x64.tar.gz
tar xvfz /tmp/nodejs/node-v0.12.4-linux-x64.tar.gz
sudo mv /tmp/nodejs/node-v0.12.4-linux-x64 /opt/
sudo ln -s /opt/node-v0.12.4-linux-x64/bin/node /usr/bin/node
sudo ln -s /opt/node-v0.12.4-linux-x64/bin/npm /usr/bin/npm
rm -Rf /tmp/nodejs

# Install global Node modules
sudo npm install -g npm bower grunt-cli gulp jshint yo
