" ===== Plugins
set nocompatible
set runtimepath+=$HOME/.vim/bundle/Vundle.vim

filetype off

call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-sensible'
Plugin 'altercation/vim-colors-solarized'
Plugin 'editorconfig/editorconfig-vim'

call vundle#end()

" ===== Defaults
filetype plugin indent on

set hidden
set noswapfile
set shell=bash
set splitbelow
set splitright
set wildignorecase

" ===== Appearance
colorscheme solarized

set background=dark
set number
