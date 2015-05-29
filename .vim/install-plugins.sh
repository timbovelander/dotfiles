#!/bin/sh

# Install Pathogen
mkdir -p $HOME/.vim/bundle
git clone https://github.com/tpope/vim-pathogen.git $HOME/.vim

# Install Plugins
cd $HOME/.vim/bundle
git clone https://github.com/altercation/vim-colors-solarized.git
git clone https://github.com/kien/ctrlp.vim
git clone https://github.com/bling/vim-airline.git
git clone https://github.com/tpope/vim-fugitive.git
git clone https://github.com/editorconfig/editorconfig-vim.git
git clone https://github.com/hail2u/vim-css3-syntax.git
git clone https://github.com/othree/html5.vim.git
git clone https://github.com/mustache/vim-mustache-handlebars.git
git clone https://github.com/mattn/emmet-vim.git
git clone https://github.com/Valloric/YouCompleteMe.git
git clone https://github.com/marijnh/tern_for_vim.git
git clone https://github.com/Raimondi/delimitMate.git
git clone https://github.com/Valloric/MatchTagAlways.git
git clone https://github.com/tpope/vim-surround.git
git clone https://github.com/scrooloose/syntastic.git
git clone https://github.com/tpope/vim-rvm.git
git clone https://github.com/chrisbra/Colorizer.git

# Build YouCompleteMe
cd $HOME/.vim/bundle/YouCompleteMe
git submodule update --init --recursive
mkdir /tmp/ycm-build && cd /tmp/ycm-build
cmake -G "Unix Makefiles" . $HOME/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp
make ycm_support_libs

# Build tern_for_vim
cd $HOME/.vim/bundle/tern_for_vim
npm install
